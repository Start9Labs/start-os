import { Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { CifsModal } from 'src/app/modals/cifs-modal/cifs-modal.page'
import { ApiService, DiskBackupTarget } from 'src/app/services/api/api.service'
import { ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  loading = true
  mappedDrives: MappedDisk[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalController: ModalController,
    private readonly errToastService: ErrorToastService,
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

  driveClickable(mapped: MappedDisk) {
    return mapped.drive['embassy-os']?.full
  }

  async getDrives() {
    this.mappedDrives = []
    try {
      const disks = await this.apiService.getDrives()
      disks
        .filter(d => d.partitions.length)
        .forEach(d => {
          d.partitions.forEach(p => {
            const drive: DiskBackupTarget = {
              vendor: d.vendor,
              model: d.model,
              logicalname: p.logicalname,
              label: p.label,
              capacity: p.capacity,
              used: p.used,
              'embassy-os': p['embassy-os'],
            }
            this.mappedDrives.push({
              hasValidBackup: !!p['embassy-os']?.full,
              drive,
            })
          })
        })
    } catch (e: any) {
      this.errToastService.present(e)
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
        const { hostname, path, username, password } = res.data.cifs
        this.stateService.recoverySource = {
          type: 'backup',
          target: {
            type: 'cifs',
            hostname,
            path,
            username,
            password,
          },
        }
        this.stateService.recoveryPassword = res.data.recoveryPassword
        this.navCtrl.navigateForward('/storage')
      }
    })
    await modal.present()
  }

  async select(target: DiskBackupTarget) {
    const { logicalname } = target

    if (!logicalname) return

    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: { target },
      cssClass: 'alertlike-modal',
    })
    modal.onDidDismiss().then(res => {
      if (res.data?.password) {
        this.selectRecoverySource(logicalname, res.data.password)
      }
    })
    await modal.present()
  }

  private async selectRecoverySource(logicalname: string, password?: string) {
    this.stateService.recoverySource = {
      type: 'backup',
      target: {
        type: 'disk',
        logicalname,
      },
    }
    this.stateService.recoveryPassword = password
    this.navCtrl.navigateForward(`/storage`)
  }
}

@Component({
  selector: 'drive-status',
  templateUrl: './drive-status.component.html',
  styleUrls: ['./recover.page.scss'],
})
export class DriveStatusComponent {
  @Input() hasValidBackup!: boolean
}

interface MappedDisk {
  hasValidBackup: boolean
  drive: DiskBackupTarget
}
