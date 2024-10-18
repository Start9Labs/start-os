import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  PasswordPromptComponent,
  PromptOptions,
} from 'src/app/modals/password-prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import { skip, takeUntil } from 'rxjs/operators'
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
  providers: [TuiDestroyService],
})
export class ServerBackupPage {
  serviceIds: string[] = []

  readonly backingUp$ = this.eosService.backingUp$

  constructor(
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
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
      title: 'Master Password Needed',
      message: 'Enter your master password to encrypt this backup.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      buttonText: 'Create Backup',
    }

    const modal = await this.modalCtrl.create({
      component: PasswordPromptComponent,
      componentProps: { options },
      canDismiss: async password => {
        if (password === null) {
          return true
        }

        const { passwordHash, id } = await getServerInfo(this.patch)

        // confirm password matches current master password
        try {
          argon2.verify(passwordHash, password)
        } catch (e: any) {
          this.errorService.handleError(e)
          return false
        }

        // first time backup
        if (!this.backupService.hasThisBackup(target.entry, id)) {
          this.createBackup(target, password)
          return true
          // existing backup
        } else {
          try {
            argon2.verify(target.entry.startOs[id].passwordHash!, password)
          } catch {
            setTimeout(
              () => this.presentModalOldPassword(target, password),
              250,
            )
            return true
          }
          await this.createBackup(target, password)
          return true
        }
      },
    })
    modal.present()
  }

  private async presentModalOldPassword(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
    password: string,
  ): Promise<void> {
    const { id } = await getServerInfo(this.patch)
    const options: PromptOptions = {
      title: 'Original Password Needed',
      message:
        'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
      label: 'Original Password',
      placeholder: 'Enter original password',
      buttonText: 'Create Backup',
    }

    const modal = await this.modalCtrl.create({
      component: PasswordPromptComponent,
      componentProps: { options },
      canDismiss: async oldPassword => {
        if (oldPassword === null) {
          return true
        }

        try {
          argon2.verify(target.entry.startOs[id].passwordHash!, oldPassword)
          await this.createBackup(target, password, oldPassword)
          return true
        } catch (e: any) {
          this.errorService.handleError(e)
          return false
        }
      },
    })
    modal.present()
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
