import { Component } from '@angular/core'
import {
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { DiskInfo, ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from 'src/app/modals/password/password.page'

@Component({
  selector: 'app-attach',
  templateUrl: 'attach.page.html',
  styleUrls: ['attach.page.scss'],
})
export class AttachPage {
  loading = true
  drives: DiskInfo[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly errToastService: ErrorToastService,
    private readonly stateService: StateService,
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      this.drives = await this.apiService.getDrives()
    } catch (e: any) {
      this.errToastService.present(e)
    } finally {
      this.loading = false
    }
  }

  async select(guid: string) {
    const modal = await this.modalCtrl.create({
      component: PasswordPage,
      componentProps: { storageDrive: true },
    })
    modal.onDidDismiss().then(res => {
      if (res.data && res.data.password) {
        this.attachDrive(guid, res.data.password)
      }
    })
    await modal.present()
  }

  private async attachDrive(guid: string, password: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Connecting to drive...',
      cssClass: 'loader',
    })
    await loader.present()
    try {
      await this.stateService.importDrive(guid, password)
      await this.navCtrl.navigateForward(`/loading`)
    } catch (e: any) {
      this.errToastService.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
