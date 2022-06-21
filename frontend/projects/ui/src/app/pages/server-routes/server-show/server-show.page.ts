import { Component } from '@angular/core'
import {
  AlertController,
  LoadingController,
  NavController,
  IonicSafeString,
  ModalController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActivatedRoute } from '@angular/router'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Observable, of } from 'rxjs'
import { filter, take } from 'rxjs/operators'
import { exists, isEmptyObject, ErrorToastService } from '@start9labs/shared'
import { EOSService } from 'src/app/services/eos.service'
import { LocalStorageService } from 'src/app/services/local-storage.service'
import { RecoveredPackageDataEntry } from 'src/app/services/patch-db/data-model'
import { OSUpdatePage } from 'src/app/modals/os-update/os-update.page'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  hasRecoveredPackage: boolean
  clicks = 0

  readonly server$ = this.patch.watch$('server-info')
  readonly ui$ = this.patch.watch$('ui')
  readonly connected$ = this.patch.connected$

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDbService,
    public readonly eosService: EOSService,
    public readonly localStorageService: LocalStorageService,
  ) {}

  ngOnInit() {
    this.patch
      .watch$('recovered-packages')
      .pipe(filter(exists), take(1))
      .subscribe((rps: { [id: string]: RecoveredPackageDataEntry }) => {
        this.hasRecoveredPackage = !isEmptyObject(rps)
      })
  }

  async updateEos(): Promise<void> {
    if (this.hasRecoveredPackage) {
      const alert = await this.alertCtrl.create({
        header: 'Cannot Update',
        message:
          'You cannot update EmbassyOS when you have unresolved recovered services.',
        buttons: ['OK'],
      })
      await alert.present()
    } else {
      const modal = await this.modalCtrl.create({
        componentProps: {
          releaseNotes: this.eosService.eos['release-notes'],
        },
        component: OSUpdatePage,
      })
      modal.present()
    }
  }

  async presentAlertRestart() {
    const alert = await this.alertCtrl.create({
      header: 'Restart',
      message:
        'Are you sure you want to restart your Embassy? It can take several minutes to come back online.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Restart',
          handler: () => {
            this.restart()
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async presentAlertShutdown() {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message:
        'Are you sure you want to power down your Embassy? This can take several minutes, and your Embassy will not come back online automatically. To power on again, You will need to physically unplug your Embassy and plug it back in',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Shutdown',
          handler: () => {
            this.shutdown()
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  async presentAlertSystemRebuild() {
    const minutes = Object.keys(this.patch.getData()['package-data']).length * 2
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message: new IonicSafeString(
        `This action will tear down all service containers and rebuild them from scratch. No data will be deleted. This action is useful if your system gets into a bad state, and it should only be performed if you are experiencing general performance or reliability issues. It may take up to ${minutes} minutes to complete. During this time, you will lose all connectivity to your Embassy.`,
      ),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Rebuild',
          handler: () => {
            this.systemRebuild()
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  async presentAlertRepairDisk() {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message: new IonicSafeString(
        `<p>This action will attempt to preform a disk repair operation and system reboot. No data will be deleted. This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action.</p><p>If anything happens to the device during the reboot (between the bep and chime), such as loosing power, a power surge, unplugging the drive, or unplugging the Embassy, the filesystem <i>will</i> be in an unrecoverable state. Please proceed with caution.</p>`,
      ),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Repair',
          handler: () => {
            try {
              this.embassyApi.repairDisk({}).then(_ => {
                this.restart()
              })
            } catch (e: any) {
              this.errToast.present(e)
            }
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  private async restart() {
    const loader = await this.loadingCtrl.create({
      message: 'Restarting...',
    })
    await loader.present()

    try {
      await this.embassyApi.restartServer({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async shutdown() {
    const loader = await this.loadingCtrl.create({
      message: 'Shutting down...',
    })
    await loader.present()

    try {
      await this.embassyApi.shutdownServer({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async systemRebuild() {
    const loader = await this.loadingCtrl.create({
      message: 'Hard Restarting...',
    })
    await loader.present()

    try {
      await this.embassyApi.systemRebuild({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async checkForEosUpdate(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Checking for updates',
    })
    await loader.present()

    try {
      const updateAvailable = await this.eosService.getEOS()

      await loader.dismiss()

      if (updateAvailable) {
        this.updateEos()
      } else {
        this.presentAlertLatest()
      }
    } catch (e: any) {
      await loader.dismiss()
      this.errToast.present(e)
    }
  }

  async presentAlertLatest() {
    const alert = await this.alertCtrl.create({
      header: 'Up to date!',
      message: 'You are on the latest version of EmbassyOS.',
      buttons: [
        {
          text: 'OK',
          role: 'cancel',
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-success-message',
    })
    alert.present()
  }

  settings: ServerSettings = {
    Backups: [
      {
        title: 'Create Backup',
        description: 'Back up your Embassy and all its services',
        icon: 'save-outline',
        action: () =>
          this.navCtrl.navigateForward(['backup'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Restore From Backup',
        description: 'Restore one or more services from a prior backup',
        icon: 'color-wand-outline',
        action: () =>
          this.navCtrl.navigateForward(['restore'], { relativeTo: this.route }),
        detail: true,
        disabled: this.eosService.updatingOrBackingUp$,
      },
    ],
    Settings: [
      {
        title: 'Software Update',
        description: 'Get the latest version of EmbassyOS',
        icon: 'cog-outline',
        action: () =>
          this.eosService.updateAvailable$.getValue()
            ? this.updateEos()
            : this.checkForEosUpdate(),
        detail: false,
        disabled: this.eosService.updatingOrBackingUp$,
      },
      {
        title: 'Preferences',
        description: 'Device name, background tasks',
        icon: 'options-outline',
        action: () =>
          this.navCtrl.navigateForward(['preferences'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'LAN',
        description: 'Access your Embassy on the Local Area Network',
        icon: 'home-outline',
        action: () =>
          this.navCtrl.navigateForward(['lan'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'SSH',
        description: 'Access your Embassy from the command line',
        icon: 'terminal-outline',
        action: () =>
          this.navCtrl.navigateForward(['ssh'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'WiFi',
        description: 'Add or remove WiFi networks',
        icon: 'wifi',
        action: () =>
          this.navCtrl.navigateForward(['wifi'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Sideload Service',
        description: `Manually install any service package`,
        icon: 'push-outline',
        action: () =>
          this.navCtrl.navigateForward(['sideload'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Marketplace Settings',
        description: 'Add or remove marketplaces',
        icon: 'storefront-outline',
        action: () =>
          this.navCtrl.navigateForward(['marketplaces'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled: of(false),
      },
    ],
    Insights: [
      {
        title: 'About',
        description: 'Basic information about your Embassy',
        icon: 'information-circle-outline',
        action: () =>
          this.navCtrl.navigateForward(['specs'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Monitor',
        description: 'CPU, disk, memory, and other useful metrics',
        icon: 'pulse',
        action: () =>
          this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Active Sessions',
        description: 'View and manage device access',
        icon: 'desktop-outline',
        action: () =>
          this.navCtrl.navigateForward(['sessions'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'OS Logs',
        description: 'Raw, unfiltered operating system logs',
        icon: 'newspaper-outline',
        action: () =>
          this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Kernel Logs',
        description:
          'Diagnostic log stream for device drivers and other kernel processes',
        icon: 'receipt-outline',
        action: () =>
          this.navCtrl.navigateForward(['kernel-logs'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled: of(false),
      },
    ],
    Support: [
      {
        title: 'User Manual',
        description: 'View the Embassy user manual and FAQ',
        icon: 'map-outline',
        action: () =>
          window.open(
            'https://start9.com/latest/user-manual/',
            '_blank',
            'noreferrer',
          ),
        detail: true,
        disabled: of(false),
      },
      {
        title: 'Contact Support',
        description: 'Get help from the Start9 team and community',
        icon: 'chatbubbles-outline',
        action: () =>
          window.open(
            'https://start9.com/latest/support/contact/',
            '_blank',
            'noreferrer',
          ),
        detail: true,
        disabled: of(false),
      },
    ],
    Power: [
      {
        title: 'Restart',
        description: '',
        icon: 'reload',
        action: () => this.presentAlertRestart(),
        detail: false,
        disabled: of(false),
      },
      {
        title: 'Shutdown',
        description: '',
        icon: 'power',
        action: () => this.presentAlertShutdown(),
        detail: false,
        disabled: of(false),
      },
      {
        title: 'System Rebuild',
        description: '',
        icon: 'construct-outline',
        action: () => this.presentAlertSystemRebuild(),
        detail: false,
        disabled: of(false),
      },
      {
        title: 'Repair Disk',
        description: '',
        icon: 'medkit-outline',
        action: () => this.presentAlertRepairDisk(),
        detail: false,
        disabled: of(false),
      },
    ],
  }

  asIsOrder() {
    return 0
  }

  async addClick() {
    this.clicks++
    if (this.clicks >= 5) {
      this.clicks = 0
      const newVal = await this.localStorageService.toggleShowDiskRepair()
    }
    setTimeout(() => {
      this.clicks = Math.max(this.clicks - 1, 0)
    }, 10000)
  }
}

interface ServerSettings {
  [key: string]: SettingBtn[]
}

interface SettingBtn {
  title: string
  description: string
  icon: string
  action: Function
  detail: boolean
  disabled: Observable<boolean>
}
