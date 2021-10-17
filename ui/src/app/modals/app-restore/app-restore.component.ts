import { Component, Input } from '@angular/core'
import { ModalController, IonicSafeString, AlertController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { GenericInputComponent } from 'src/app/modals/generic-input/generic-input.component'
import { getErrorMessage } from 'src/app/services/error-toast.service'
import { MappedDiskInfo, MappedPartitionInfo } from 'src/app/util/misc.util'
import { Emver } from 'src/app/services/emver.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'app-restore',
  templateUrl: './app-restore.component.html',
  styleUrls: ['./app-restore.component.scss'],
})
export class AppRestoreComponent {
  @Input() pkg: PackageDataEntry
  disks: MappedDiskInfo[]
  loading = true
  modal: HTMLIonModalElement
  loadingError: string | IonicSafeString

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
    private readonly config: ConfigService,
    private readonly emver: Emver,
  ) { }

  async ngOnInit () {
    this.getExternalDisks()
    this.modal = await this.modalCtrl.getTop()
  }

  async refresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      const disks = await this.embassyApi.getDisks({ })
      this.disks = disks.map(d => {
        const partionInfo: MappedPartitionInfo[] = d.partitions.map(p => {
          return {
            ...p,
            hasBackup: [0, 1].includes(this.emver.compare(p['embassy-os']?.version, '0.3.0')),
            backupInfo: null,
          }
        })
        return {
          ...d,
          partitions: partionInfo,
        }
      })
    } catch (e) {
      this.loadingError = getErrorMessage(e)
    } finally {
      this.loading = false
    }
  }

  async presentModal (logicalname: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        title: 'Decryption Required',
        message: 'Enter the password that was originally used to encrypt this backup.',
        warning: `Warning! All current data for ${this.pkg.manifest.title} will be overwritten.`,
        label: 'Password',
        placeholder: 'Enter password',
        useMask: true,
        buttonText: 'Restore',
        loadingText: 'Decrypting drive...',
        submitFn: (value: string, loader: HTMLIonLoadingElement) => this.restore(logicalname, value, loader),
      },
      cssClass: 'alertlike-modal',
      presentingElement: await this.modalCtrl.getTop(),
      component: GenericInputComponent,
    })

    modal.onWillDismiss().then(res => {
      if (res.role === 'success') this.modal.dismiss(undefined, 'success')
    })

    await modal.present()
  }

  dismiss () {
    this.modalCtrl.dismiss()
  }

  private async restore (logicalname: string, password: string, loader: HTMLIonLoadingElement): Promise<void> {
    const { id, title } = this.pkg.manifest

    const backupInfo = await this.embassyApi.getBackupInfo({
      logicalname,
      password,
    })
    const pkgBackupInfo = backupInfo['package-backups'][id]

    if (!pkgBackupInfo) {
      throw new Error(`Disk does not contain a backup of ${title}`)
    }

    if (this.emver.compare(pkgBackupInfo['os-version'], this.config.version) === 1) {
      throw new Error(`The backup of ${title} you are attempting to restore was made on a newer version of EmbassyOS. Update EmbassyOS and try again.`)
    }

    const timestamp = new Date(pkgBackupInfo.timestamp).getTime()
    const lastBackup = new Date(this.pkg.installed['last-backup']).getTime() // ok if last-backup is null
    if (timestamp < lastBackup) {
      const proceed = await this.presentAlertNewerBackup()
      if (!proceed) {
        throw new Error('Action cancelled')
      }
    }

    loader.message = `Beginning Restore of ${title}`

    await this.embassyApi.restorePackage({
      id,
      logicalname,
      password,
    })
  }

  private async presentAlertNewerBackup (): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Outdated Backup',
        message: `The backup you are attempting to restore is older than your most recent backup of ${this.pkg.manifest.title}. Are you sure you want to continue?`,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => resolve(false),
          },
          {
            text: 'Continue',
            handler: () => resolve(true),
            cssClass: 'enter-click',
          },
        ],
      })
      await alert.present()
    })
  }
}
