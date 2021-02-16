import { Component } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { BackupConfirmationComponent } from 'src/app/modals/backup-confirmation/backup-confirmation.component'
import { DiskInfo, PartitionInfoEntry } from 'src/app/services/api/api-types'
import { ActivatedRoute } from '@angular/router'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'

@Component({
  selector: 'app-restore',
  templateUrl: './app-restore.page.html',
  styleUrls: ['./app-restore.page.scss'],
})
export class AppRestorePage {
  disks: DiskInfo
  pkgId: string
  loading = true
  error: string
  allPartitionsMounted: boolean

  constructor (
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly loadingCtrl: LoadingController,
    public readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.getExternalDisks()
  }

  async doRefresh () {
    this.loading = true
    await this.getExternalDisks()
  }

  async getExternalDisks (): Promise<void> {
    try {
      this.disks = await this.apiService.getDisks({ })
      this.allPartitionsMounted = Object.values(this.disks).every(d => Object.values(d.partitions).every(p => p['is-mounted']))
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.loading = false
    }
  }

  async presentModal (logicalname: string, partition: PartitionInfoEntry): Promise<void> {
    const m = await this.modalCtrl.create({
      componentProps: {
        name: partition.label || logicalname,
      },
      cssClass: 'alertlike-modal',
      component: BackupConfirmationComponent,
      backdropDismiss: false,
    })

    m.onWillDismiss().then(res => {
      const data = res.data
      if (data.cancel) return
      this.restore(logicalname, data.password)
    })

    return await m.present()
  }

  private async restore (logicalname: string, password: string): Promise<void> {
    this.error = ''

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
    })
    await loader.present()

    try {
      await this.apiService.restorePackage({
        id: this.pkgId,
        logicalname,
        password,
      })
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }
}
