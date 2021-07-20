import { Component } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { BackupConfirmationComponent } from 'src/app/modals/backup-confirmation/backup-confirmation.component'
import { DiskInfo } from 'src/app/services/api/api.types'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'server-backup',
  templateUrl: './server-backup.page.html',
  styleUrls: ['./server-backup.page.scss'],
})
export class ServerBackupPage {
  disks: DiskInfo
  loading = true
  allPartitionsMounted: boolean

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
  ) { }

  ngOnInit () {
    this.getExternalDisks()
  }

  async doRefresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      this.disks = await this.embassyApi.getDisks({ })
      this.allPartitionsMounted = Object.values(this.disks).every(d => Object.values(d.partitions).every(p => p['is-mounted']))
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }

  async presentModal (logicalname: string): Promise<void> {
    const m = await this.modalCtrl.create({
      componentProps: {
        type: 'backup',
      },
      cssClass: 'alertlike-modal',
      component: BackupConfirmationComponent,
      backdropDismiss: false,
    })

    m.onWillDismiss().then(res => {
      const data = res.data
      if (data.cancel) return
      this.create(logicalname, data.password)
    })

    return await m.present()
  }

  private async create (logicalname: string, password: string): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Starting backup...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.createBackup({ logicalname, password })
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
