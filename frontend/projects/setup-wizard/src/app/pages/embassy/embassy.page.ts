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
import { DiskInfo, ErrorToastService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'
import { ActivatedRoute } from '@angular/router'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
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
    private route: ActivatedRoute,
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
    if (!!drive.partitions.find(p => p.used) || !!drive.guid) {
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
      message: 'Initializing data drive. This could take a while...',
    })

    await loader.present()

    try {
      await this.stateService.setupEmbassy(logicalname, password)
      if (!!this.stateService.recoverySource) {
        await this.navCtrl.navigateForward(`/loading`, {
          queryParams: { action: this.route.snapshot.paramMap.get('action') },
        })
      } else {
        await this.navCtrl.navigateForward(`/success`)
      }
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
