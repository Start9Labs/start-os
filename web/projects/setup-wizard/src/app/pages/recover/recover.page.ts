import { Component } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { ErrorService } from '@start9labs/shared'
import { CifsModal } from 'src/app/modals/cifs-modal/cifs-modal.page'
import {
  ApiService,
  StartOSDiskInfoFull,
} from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  loading = true
  servers: StartOSDiskInfoFull[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalController: ModalController,
    private readonly errorService: ErrorService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    this.stateService.setupType = 'restore'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    try {
      const drives = await this.apiService.getDrives()
      this.servers = drives.flatMap(drive =>
        drive.partitions.flatMap(partition =>
          Object.entries(partition.startOs).map(([id, val]) => ({
            id,
            ...val,
            partition,
            drive,
          })),
        ),
      )
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  async presentModalCifs(): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: CifsModal,
    })
    modal.onDidDismiss().then(res => {
      if (res.role === 'success') {
        this.stateService.recoverySource = {
          type: 'backup',
          target: {
            type: 'cifs',
            ...res.data.cifs,
          },
          serverId: res.data.serverId,
          password: res.data.recoveryPassword,
        }
        this.navCtrl.navigateForward('/storage')
      }
    })
    await modal.present()
  }

  async select(server: StartOSDiskInfoFull) {
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: { passwordHash: server.passwordHash },
      cssClass: 'alertlike-modal',
    })
    modal.onDidDismiss().then(res => {
      if (res.role === 'success') {
        this.stateService.recoverySource = {
          type: 'backup',
          target: {
            type: 'disk',
            logicalname: server.partition.logicalname,
          },
          serverId: server.id,
          password: res.data.password,
        }
        this.navCtrl.navigateForward(`/storage`)
      }
    })
    await modal.present()
  }
}
