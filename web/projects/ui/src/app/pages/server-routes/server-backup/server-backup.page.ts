import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PROMPT, PromptOptions } from 'src/app/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { skip, take, takeUntil } from 'rxjs/operators'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import * as argon2 from '@start9labs/argon2'
import { TuiDestroyService } from '@taiga-ui/cdk'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { BackupSelectPage } from 'src/app/modals/backup-select/backup-select.page'
import { EOSService } from 'src/app/services/eos.service'
import { getServerInfo } from 'src/app/util/get-server-info'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { BackupService } from 'src/app/components/backup-drives/backup.service'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
  providers: [TuiDestroyService],
})
export class ServerBackupPage {
  serviceIds: string[] = []

  readonly backingUp$ = this.eosService.backingUp$

  constructor(
    private readonly loader: LoadingService,
    private readonly dialogs: TuiDialogService,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly destroy$: TuiDestroyService,
    private readonly eosService: EOSService,
    private readonly patch: PatchDB<DataModel>,
    private readonly backupService: BackupService,
  ) {}

  ngOnInit() {
    this.backingUp$
      .pipe(skip(1), takeUntil(this.destroy$))
      .subscribe(isBackingUp => {
        if (!isBackingUp) {
          this.navCtrl.navigateRoot('/system')
        }
      })
  }

  async presentModalSelect(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ) {
    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupSelectPage,
    })

    modal.onDidDismiss().then(res => {
      if (res.data) {
        this.serviceIds = res.data
        this.presentModalPassword(target)
      }
    })

    await modal.present()
  }

  private async presentModalPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): Promise<void> {
    const options: PromptOptions = {
      message: 'Enter your master password to encrypt this backup.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Create Backup',
    }

    this.dialogs
      .open<string>(PROMPT, {
        label: 'Master Password Needed',
        data: options,
      })
      .pipe(take(1))
      .subscribe(async (password: string) => {
        const { passwordHash, id } = await getServerInfo(this.patch)

        // @TODO Alex if invalid password, we should tell the user "Invalid password" and halt execution of this function. The modal should remain so the user can try again. Correct password is asdfasdf
        // confirm password matches current master password
        argon2.verify(passwordHash, password)

        // first time backup
        if (!this.backupService.hasThisBackup(target.entry, id)) {
          await this.createBackup(target, password)
          // existing backup
        } else {
          try {
            argon2.verify(target.entry.startOs[id].passwordHash!, password)
          } catch {
            setTimeout(
              () => this.presentModalOldPassword(target, password),
              250,
            )
            return
          }
          await this.createBackup(target, password)
        }
      })
  }

  private async presentModalOldPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
  ): Promise<void> {
    const options: PromptOptions = {
      message:
        'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
      label: 'Original Password',
      placeholder: 'Enter original password',
      useMask: true,
      buttonText: 'Create Backup',
    }

    const { id } = await getServerInfo(this.patch)

    this.dialogs
      .open<string>(PROMPT, {
        label: 'Original Password Needed',
        data: options,
      })
      .pipe(take(1))
      .subscribe(async (oldPassword: string) => {
        // @TODO Alex if invalid password, we should tell the user "Invalid password" and halt execution of this function. The modal should remain so the user can try again. Correct password is asdfasdf
        argon2.verify(target.entry.startOs[id].passwordHash!, oldPassword)
        await this.createBackup(target, password, oldPassword)
      })
  }

  private async createBackup(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const loader = this.loader.open('Beginning backup...').subscribe()

    try {
      await this.embassyApi.createBackup({
        targetId: target.id,
        packageIds: this.serviceIds,
        oldPassword: oldPassword || null,
        password,
      })
    } finally {
      loader.unsubscribe()
    }
  }
}
