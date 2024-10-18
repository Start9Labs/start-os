import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { DiskInfo, ErrorService, GuidPipe } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PasswordPage } from '../../modals/password/password.page'
import { T } from '@start9labs/start-sdk'

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
    private readonly errorService: ErrorService,
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
      if (this.stateService.setupType === 'fresh') {
        this.storageDrives = disks
      } else if (
        this.stateService.setupType === 'restore' &&
        this.stateService.recoverySource?.type === 'backup'
      ) {
        if (this.stateService.recoverySource.target.type === 'disk') {
          const logicalname =
            this.stateService.recoverySource.target.logicalname
          this.storageDrives = disks.filter(
            d => !d.partitions.map(p => p.logicalname).includes(logicalname),
          )
        } else {
          this.storageDrives = disks
        }
      } else if (
        this.stateService.setupType === 'transfer' &&
        this.stateService.recoverySource?.type === 'migrate'
      ) {
        const guid = this.stateService.recoverySource.guid
        this.storageDrives = disks.filter(d => {
          return (
            d.guid !== guid && !d.partitions.map(p => p.guid).includes(guid)
          )
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
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
              if (this.stateService.recoverySource?.type === 'backup') {
                this.setupEmbassy(
                  drive.logicalname,
                  this.stateService.recoverySource.password,
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
      if (this.stateService.recoverySource?.type === 'backup') {
        this.setupEmbassy(
          drive.logicalname,
          this.stateService.recoverySource.password,
        )
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
      this.errorService.handleError(e)
    } finally {
      loader.dismiss()
    }
  }
}

function isDiskRecovery(source: T.RecoverySource<string>): source is any {
  return source.type === 'backup' && source.target.type === 'disk'
}
