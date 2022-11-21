import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import {
  ApiService,
  BackupRecoverySource,
  DiskRecoverySource,
} from 'src/app/services/api/api.service'
import { DiskInfo, ErrorToastService, GuidPipe } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
  providers: [GuidPipe],
})
export class EmbassyPage {
  storageDrives: DiskInfo[] = []
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly modalController: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly stateService: StateService,
    private readonly loadingCtrl: LoadingController,
    private readonly errorToastService: ErrorToastService,
    private readonly guidPipe: GuidPipe,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  tooSmall(drive: DiskInfo) {
    return drive.capacity < 34359738368
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    this.loading = true
    try {
      const disks = await this.apiService.getDrives()
      this.storageDrives = disks.filter(
        d =>
          !d.partitions
            .map(p => p.logicalname)
            .includes(
              (
                (this.stateService.recoverySource as BackupRecoverySource)
                  ?.target as DiskRecoverySource
              )?.logicalname,
            ),
      )
    } catch (e: any) {
      this.errorToastService.present(e)
    } finally {
      this.loading = false
    }
  }

  async chooseDrive(drive: DiskInfo) {
    if (
      this.guidPipe.transform(drive) ||
      !!drive.partitions.find(p => p.used)
    ) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        subHeader: 'Drive contains data!',
        message: 'All data stored on this drive will be permanently deleted.',
        buttons: [
          {
            role: 'cancel',
            text: 'Cancel',
          },
          {
            text: 'Continue',
            handler: () => {
              // for backup recoveries
              if (this.stateService.recoveryPassword) {
                this.setupEmbassy(
                  drive.logicalname,
                  this.stateService.recoveryPassword,
                )
              } else {
                // for migrations and fresh setups
                this.presentModalPassword(drive.logicalname)
              }
            },
          },
        ],
      })
      await alert.present()
    } else {
      // for backup recoveries
      if (this.stateService.recoveryPassword) {
        this.setupEmbassy(drive.logicalname, this.stateService.recoveryPassword)
      } else {
        // for migrations and fresh setups
        this.presentModalPassword(drive.logicalname)
      }
    }
  }

  private async presentModalPassword(logicalname: string): Promise<void> {
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: {
        storageDrive: true,
      },
    })
    modal.onDidDismiss().then(async ret => {
      if (!ret.data || !ret.data.password) return
      this.setupEmbassy(logicalname, ret.data.password)
    })
    await modal.present()
  }

  private async setupEmbassy(
    logicalname: string,
    password: string,
  ): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Connecting to drive...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.stateService.setupEmbassy(logicalname, password)
      await this.navCtrl.navigateForward(`/loading`)
    } catch (e: any) {
      this.errorToastService.present({
        message: `${e.message}\n\nRestart Embassy to try again.`,
      })
      console.error(e)
    } finally {
      loader.dismiss()
    }
  }
}
