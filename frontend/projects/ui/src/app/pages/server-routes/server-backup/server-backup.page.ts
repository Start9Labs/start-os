import { Component } from '@angular/core'
import {
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  GenericInputComponent,
  GenericInputOptions,
} from 'src/app/modals/generic-input/generic-input.component'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { skip, takeUntil } from 'rxjs/operators'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import * as argon2 from '@start9labs/argon2'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { BackupSelectPage } from 'src/app/modals/backup-select/backup-select.page'
import { EOSService } from 'src/app/services/eos.service'
import { DestroyService } from '@start9labs/shared'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
  providers: [DestroyService],
})
export class ServerBackupPage {
  target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>
  serviceIds: string[] = []

  readonly backingUp$ = this.eosService.backingUp$

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly destroy$: DestroyService,
    private readonly eosService: EOSService,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    this.backingUp$
      .pipe(skip(1), takeUntil(this.destroy$))
      .subscribe(isBackingUp => {
        if (!isBackingUp) {
          this.navCtrl.navigateRoot('/embassy')
        }
      })
  }

  async presentModalSelect(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ) {
    this.target = target

    const modal = await this.modalCtrl.create({
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupSelectPage,
    })

    modal.onWillDismiss().then(res => {
      if (res.data) {
        this.serviceIds = res.data
        this.presentModalPassword()
      }
    })

    await modal.present()
  }

  async presentModalPassword(): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Master Password Needed',
      message: 'Enter your master password to encrypt this backup.',
      label: 'Master Password',
      placeholder: 'Enter master password',
      useMask: true,
      buttonText: 'Create Backup',
      submitFn: async (password: string) => {
        // confirm password matches current master password
        const passwordHash =
          this.patch.getData()['server-info']['password-hash']
        argon2.verify(passwordHash, password)

        // first time backup
        if (!this.target.hasValidBackup) {
          await this.createBackup(password)
          // existing backup
        } else {
          try {
            const passwordHash =
              this.target.entry['embassy-os']?.['password-hash'] || ''

            argon2.verify(passwordHash, password)
          } catch {
            setTimeout(() => this.presentModalOldPassword(password), 500)
            return
          }
          await this.createBackup(password)
        }
      },
    }

    const m = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      cssClass: 'alertlike-modal',
    })

    await m.present()
  }

  private async presentModalOldPassword(password: string): Promise<void> {
    const options: GenericInputOptions = {
      title: 'Original Password Needed',
      message:
        'This backup was created with a different password. Enter the ORIGINAL password that was used to encrypt this backup.',
      label: 'Original Password',
      placeholder: 'Enter original password',
      useMask: true,
      buttonText: 'Create Backup',
      submitFn: async (oldPassword: string) => {
        const passwordHash =
          this.target.entry['embassy-os']?.['password-hash'] || ''

        argon2.verify(passwordHash, oldPassword)
        await this.createBackup(password, oldPassword)
      },
    }

    const m = await this.modalCtrl.create({
      component: GenericInputComponent,
      componentProps: { options },
      cssClass: 'alertlike-modal',
    })

    await m.present()
  }

  private async createBackup(
    password: string,
    oldPassword?: string,
  ): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Beginning backup...',
    })
    await loader.present()

    try {
      await this.embassyApi.createBackup({
        'target-id': this.target.id,
        'package-ids': this.serviceIds,
        'old-password': oldPassword || null,
        password,
      })
    } finally {
      loader.dismiss()
    }
  }
}
