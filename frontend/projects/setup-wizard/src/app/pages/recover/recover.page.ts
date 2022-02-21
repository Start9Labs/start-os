import { Component, Input } from '@angular/core'
import {
  AlertController,
  IonicSafeString,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { CifsModal } from 'src/app/modals/cifs-modal/cifs-modal.page'
import { ApiService, DiskBackupTarget } from 'src/app/services/api/api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'
import { ProdKeyModal } from '../../modals/prod-key-modal/prod-key-modal.page'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  loading = true
  mappedDrives: MappedDisk[] = []
  hasShownGuidAlert = false

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalCtrl: ModalController,
    private readonly modalController: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly errorToastService: ErrorToastService,
    public readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  driveClickable(mapped: MappedDisk) {
    return (
      mapped.drive['embassy-os']?.full &&
      (this.stateService.hasProductKey || mapped.is02x)
    )
  }

  async getDrives() {
    this.mappedDrives = []
    try {
      const { disks, reconnect } = await this.apiService.getDrives()
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
              hasValidBackup: p['embassy-os']?.full,
              is02x: drive['embassy-os']?.version.startsWith('0.2'),
              drive,
            })
          })
        })

      if (!this.mappedDrives.length && reconnect.length) {
        const list = `<ul>${reconnect.map(recon => `<li>${recon}</li>`)}</ul>`
        const alert = await this.alertCtrl.create({
          header: 'Warning',
          message: `One or more devices you connected had to be reconfigured to support the current hardware platform. Please unplug and replug the following device(s), then refresh the page:<br> ${list}`,
          buttons: [
            {
              role: 'cancel',
              text: 'OK',
            },
          ],
        })
        await alert.present()
      }

      const importableDrive = disks.find(d => !!d.guid)
      if (!!importableDrive && !this.hasShownGuidAlert) {
        const alert = await this.alertCtrl.create({
          header: 'Embassy Data Drive Detected',
          message: new IonicSafeString(
            `${importableDrive.vendor || 'Unknown Vendor'} - ${
              importableDrive.model || 'Unknown Model'
            } contains Embassy data. To use this drive and its data <i>as-is</i>, click "Use Drive". This will complete the setup process.<br /><br /><b>Important</b>. If you are trying to restore from backup or update from 0.2.x, DO NOT click "Use Drive". Instead, click "Cancel" and follow instructions.`,
          ),
          buttons: [
            {
              role: 'cancel',
              text: 'Cancel',
            },
            {
              text: 'Use Drive',
              handler: async () => {
                await this.importDrive(importableDrive.guid)
              },
            },
          ],
        })
        await alert.present()
        this.hasShownGuidAlert = true
      }
    } catch (e) {
      this.errorToastService.present(e.message)
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
          type: 'cifs',
          hostname,
          path,
          username,
          password,
        }
        this.stateService.recoveryPassword = res.data.recoveryPassword
        this.navCtrl.navigateForward('/embassy')
      }
    })
    await modal.present()
  }

  async select(target: DiskBackupTarget) {
    const is02x = target['embassy-os'].version.startsWith('0.2')

    if (this.stateService.hasProductKey) {
      if (is02x) {
        this.selectRecoverySource(target.logicalname)
      } else {
        const modal = await this.modalController.create({
          component: PasswordPage,
          componentProps: { target },
          cssClass: 'alertlike-modal',
        })
        modal.onDidDismiss().then(res => {
          if (res.data && res.data.password) {
            this.selectRecoverySource(target.logicalname, res.data.password)
          }
        })
        await modal.present()
      }
      // if no product key, it means they are an upgrade kit user
    } else {
      if (!is02x) {
        const alert = await this.alertCtrl.create({
          header: 'Error',
          message:
            'In order to use this image, you must select a drive containing a valid 0.2.x Embassy.',
          buttons: [
            {
              role: 'cancel',
              text: 'OK',
            },
          ],
        })
        await alert.present()
      } else {
        const modal = await this.modalController.create({
          component: ProdKeyModal,
          componentProps: { target },
          cssClass: 'alertlike-modal',
        })
        modal.onDidDismiss().then(res => {
          if (res.data && res.data.productKey) {
            this.selectRecoverySource(target.logicalname)
          }
        })
        await modal.present()
      }
    }
  }

  private async importDrive(guid: string) {
    const loader = await this.loadingCtrl.create({
      message: 'Importing Drive',
    })
    await loader.present()
    try {
      await this.stateService.importDrive(guid)
      await this.navCtrl.navigateForward(`/init`)
    } catch (e) {
      this.errorToastService.present(e.message)
    } finally {
      loader.dismiss()
    }
  }

  private async selectRecoverySource(logicalname: string, password?: string) {
    this.stateService.recoverySource = {
      type: 'disk',
      logicalname,
    }
    this.stateService.recoveryPassword = password
    this.navCtrl.navigateForward(`/embassy`)
  }
}

@Component({
  selector: 'drive-status',
  templateUrl: './drive-status.component.html',
  styleUrls: ['./recover.page.scss'],
})
export class DriveStatusComponent {
  @Input() hasValidBackup: boolean
  @Input() is02x: boolean
}

interface MappedDisk {
  is02x: boolean
  hasValidBackup: boolean
  drive: DiskBackupTarget
}
