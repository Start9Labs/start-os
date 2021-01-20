import { Component, Input } from '@angular/core'
import { ModalController, AlertController, LoadingController, ToastController } from '@ionic/angular'
import { AppModel, AppStatus } from 'src/app/models/app-model'
import { AppInstalledFull } from 'src/app/models/app-types'
import { ApiService } from 'src/app/services/api/api.service'
import { DiskInfo, DiskPartition } from 'src/app/models/server-model'
import { pauseFor } from 'src/app/util/misc.util'
import { concatMap } from 'rxjs/operators'
import { AppBackupConfirmationComponent } from 'src/app/components/app-backup-confirmation/app-backup-confirmation.component'

@Component({
  selector: 'app-backup',
  templateUrl: './app-backup.page.html',
  styleUrls: ['./app-backup.page.scss'],
})
export class AppBackupPage {
  @Input() app: AppInstalledFull
  @Input() type: 'create' | 'restore'
  disks: DiskInfo[]
  loading = true
  error: string
  allPartitionsMounted: boolean

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly apiService: ApiService,
    private readonly appModel: AppModel,
    private readonly toastCtrl: ToastController,
  ) { }

  ngOnInit () {
    return this.getExternalDisks().then(() => this.loading = false)
  }

  async getExternalDisks (): Promise<void> {
    try {
      this.disks = await this.apiService.getExternalDisks()
      this.allPartitionsMounted = this.disks.every(d => d.partitions.every(p => p.isMounted))
    } catch (e) {
      console.error(e)
      this.error = e.message
    }
  }

  async doRefresh (event: any) {
    await Promise.all([
      this.getExternalDisks(),
      pauseFor(600),
    ])
    event.target.complete()
  }

  async dismiss () {
    await this.modalCtrl.dismiss()
  }

  async presentAlertHelp (): Promise<void> {
    let alert: HTMLIonAlertElement
    if (this.type === 'create') {
      alert = await this.alertCtrl.create({
        backdropDismiss: false,
        header: `Backups`,
        message: `Select a location to back up ${this.app.title}.<br /><br />Internal drives and drives currently backing up other services will not be available.<br /><br />Depending on the amount of data in ${this.app.title}, your first backup may take a while. Since backups are diff-based, the speed of future backups to the same disk will likely be much faster.`,
        buttons: ['Dismiss'],
      })
    } else if (this.type === 'restore') {
      alert = await this.alertCtrl.create({
        backdropDismiss: false,
        header: `Backups`,
        message: `Select a location containing the backup you wish to restore for ${this.app.title}.<br /><br />Restoring ${this.app.title} will re-sync your service with your previous backup. The speed of the restore process depends on the backup size.`,
        buttons: ['Dismiss'],
      })
    }
    await alert.present()
  }

  async presentAlert (disk: DiskInfo, partition: DiskPartition): Promise<void> {
    if (this.type === 'create') {
      this.presentAlertCreateEncrypted(disk, partition)
    } else {
      this.presentAlertWarn(partition)
    }
  }

  private async presentAlertCreateEncrypted (disk: DiskInfo, partition: DiskPartition): Promise<void> {
    const m = await this.modalCtrl.create({
      componentProps: {
        app: this.app,
        partition,
      },
      cssClass: 'alertlike-modal',
      component: AppBackupConfirmationComponent,
      backdropDismiss: false,
    })

    m.onWillDismiss().then(res => {
      const data = res.data
      if (data.cancel) return
      // we hard code the 'eject' last argument to be false, until ejection is an option in the UI.
      return this.create(disk, partition, data.password, false)
    })

    return await m.present()
  }

  private async presentAlertWarn (partition: DiskPartition): Promise<void> {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: `Warning`,
      message: `Restoring ${this.app.title} from "${partition.label || partition.logicalname}" will overwrite its current data.<br /><br />Are you sure you want to continue?`,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        }, {
          text: 'Continue',
          handler: () => {
            this.presentAlertRestore(partition)
          },
        },
      ],
    })
    await alert.present()
  }

  private async presentAlertRestore (partition: DiskPartition): Promise<void> {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: `Decrypt Backup`,
      message: `Enter your master password`,
      inputs: [
        {
          name: 'password',
          type: 'password',
          placeholder: 'Password',
        },
      ],
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        }, {
          text: 'Restore',
          handler: (data) => {
            this.restore(partition, data.password)
          },
        },
      ],
    })
    await alert.present()
  }

  private async restore (partition: DiskPartition, password?: string): Promise<void> {
    this.error = ''

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      cssClass: 'loader-ontop-of-all',
    })
    await loader.present()

    try {
      await this.apiService.restoreAppBackup(this.app.id, partition.logicalname, password)
      this.appModel.update({ id: this.app.id, status: AppStatus.RESTORING_BACKUP })
      await this.dismiss()
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      await loader.dismiss()
    }
  }

  private async create (disk: DiskInfo, partition: DiskPartition, password: string, eject: boolean): Promise<void> {
    this.error = ''

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      cssClass: 'loader-ontop-of-all',
    })
    await loader.present()

    try {
      await this.apiService.createAppBackup(this.app.id, partition.logicalname, password)
      this.appModel.update({ id: this.app.id, status: AppStatus.CREATING_BACKUP })
      if (eject) {
        this.appModel.watchForBackup(this.app.id).pipe(concatMap(
          () => this.apiService.ejectExternalDisk(disk.logicalname),
        )).subscribe({
          next: () => this.toastEjection(disk, true),
          error: () => this.toastEjection(disk, false),
        })
      }
      await this.dismiss()
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      await loader.dismiss()
    }
  }

  private async toastEjection (disk: DiskInfo, success: boolean) {
    const { header, message, cssClass } = success ? {
      header: 'Success',
      message: `Drive ${disk.logicalname} ejected successfully`,
      cssClass: 'notification-toast',
    } : {
      header: 'Error',
      message: `Drive ${disk.logicalname} did not eject successfully`,
      cssClass: 'alert-error-message',
    }
    const t = await this.toastCtrl.create({
      header,
      message,
      cssClass,
      duration: 2000,
    })
    await t.present()
  }
}
