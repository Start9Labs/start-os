import { Component, Inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import {
  AlertController,
  ModalController,
  NavController,
  ToastController,
} from '@ionic/angular'
import { WINDOW } from '@ng-web-apis/common'
import * as argon2 from '@start9labs/argon2'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { CB } from '@start9labs/start-sdk'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter, from, Observable, of, switchMap } from 'rxjs'
import { take } from 'rxjs/operators'
import { FormComponent } from 'src/app/components/form.component'
import { OSUpdatePage } from 'src/app/modals/os-update/os-update.page'
import { PROMPT } from 'src/app/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { ConfigService } from 'src/app/services/config.service'
import { EOSService } from 'src/app/services/eos.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { getServerInfo } from 'src/app/util/get-server-info'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  manageClicks = 0
  powerClicks = 0

  readonly server$ = this.patch.watch$('serverInfo')
  readonly showUpdate$ = this.eosService.showUpdate$
  readonly showDiskRepair$ = this.ClientStorageService.showDiskRepair$
  readonly wifiConnected$ = this.patch.watch$('serverInfo', 'wifi', 'selected')

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly alerts: TuiAlertService,
    private readonly dialogs: TuiDialogService,
    private readonly formDialog: FormDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly ClientStorageService: ClientStorageService,
    private readonly authService: AuthService,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  async setBrowserTab(): Promise<void> {
    this.patch
      .watch$('ui', 'name')
      .pipe(
        switchMap(initialValue =>
          this.dialogs.open<string>(PROMPT, {
            label: 'Browser Tab Title',
            data: {
              message: `This value will be displayed as the title of your browser tab.`,
              label: 'Device Name',
              placeholder: 'StartOS',
              required: false,
              buttonText: 'Save',
              initialValue,
            },
          }),
        ),
        take(1),
      )
      .subscribe(async name => {
        const loader = this.loader.open('Saving...').subscribe()

        try {
          await this.embassyApi.setDbValue<string | null>(
            ['name'],
            name || null,
          )
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async presentAlertResetPassword() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'You will still need your current password to decrypt existing backups!',
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(
        filter(Boolean),
        switchMap(() => from(configBuilderToSpec(passwordSpec))),
      )
      .subscribe(spec => {
        this.formDialog.open(FormComponent, {
          label: 'Change Master Password',
          data: {
            spec,
            buttons: [
              {
                text: 'Save',
                handler: (value: PasswordSpec) => this.resetPassword(value),
              },
            ],
          },
        })
      })
  }

  private async resetPassword(value: PasswordSpec): Promise<boolean> {
    let err = ''

    if (value.newPassword1 !== value.newPassword2) {
      err = 'New passwords do not match'
    } else if (value.newPassword1.length < 12) {
      err = 'New password must be 12 characters or greater'
    } else if (value.newPassword1.length > 64) {
      err = 'New password must be less than 65 characters'
    }

    // confirm current password is correct
    const { passwordHash } = await getServerInfo(this.patch)
    try {
      argon2.verify(passwordHash, value.currentPassword)
    } catch (e) {
      err = 'Current password is invalid'
    }

    if (err) {
      this.errorService.handleError(err)
      return false
    }

    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.embassyApi.resetPassword({
        oldPassword: value.currentPassword,
        newPassword: value.newPassword1,
      })

      this.alerts.open('Password changed!').subscribe()

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  async presentAlertResetTor() {
    const isTor = this.config.isTor()
    const shared =
      'Optionally wipe state to forcibly acquire new guard nodes. It is recommended to try without wiping state first.'
    const alert = await this.alertCtrl.create({
      header: isTor ? 'Warning' : 'Confirm',
      message: isTor
        ? `You are currently connected over Tor. If you reset the Tor daemon, you will lose connectivity until it comes back online.<br/><br/>${shared}`
        : `Reset Tor?<br/><br/>${shared}`,
      inputs: [
        {
          label: 'Wipe state',
          type: 'checkbox',
          value: 'wipe',
        },
      ],
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Reset',
          handler: (value: string[]) => {
            this.resetTor(value.some(v => v === 'wipe'))
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: isTor ? 'alert-warning-message' : '',
    })
    await alert.present()
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open('Resetting Tor...').subscribe()

    try {
      await this.embassyApi.resetTor({
        wipeState: wipeState,
        reason: 'User triggered',
      })
      const toast = await this.toastCtrl.create({
        header: 'Tor reset in progress',
        position: 'bottom',
        duration: 4000,
        buttons: [
          {
            side: 'start',
            icon: 'close',
            handler: () => {
              return true
            },
          },
        ],
      })
      await toast.present()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async updateEos(): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: OSUpdatePage,
    })
    modal.present()
  }

  async presentAlertLogout() {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Are you sure you want to log out?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Logout',
          handler: () => this.logout(),
          cssClass: 'enter-click',
        },
      ],
    })

    await alert.present()
  }

  async presentAlertRestart() {
    const alert = await this.alertCtrl.create({
      header: 'Restart',
      message:
        'Are you sure you want to restart your server? It can take several minutes to come back online.',
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
        'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, you will need to physically unplug your server and plug it back in.',
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

  async presentAlertRepairDisk() {
    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message: `<p>This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action.</p><p>If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem <i>will</i> be in an unrecoverable state. Please proceed with caution.</p>`,
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
              this.errorService.handleError(e)
            }
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })
    await alert.present()
  }

  addClick(title: string) {
    switch (title) {
      case 'Manage':
        this.addManageClick()
        break
      case 'Power':
        this.addPowerClick()
        break
      default:
        return
    }
  }

  // should wipe cache independent of actual BE logout
  private logout() {
    this.embassyApi.logout({}).catch(e => console.error('Failed to log out', e))
    this.authService.setUnverified()
  }

  private async restart() {
    const action = 'Restart'
    const loader = this.loader.open(`Beginning ${action}...`).subscribe()

    try {
      await this.embassyApi.restartServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async shutdown() {
    const action = 'Shutdown'
    const loader = this.loader.open(`Beginning ${action}...`).subscribe()

    try {
      await this.embassyApi.shutdownServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async checkForEosUpdate(): Promise<void> {
    const loader = this.loader.open('Checking for updates').subscribe()

    try {
      await this.eosService.loadEos()

      await loader.unsubscribe()

      if (this.eosService.updateAvailable$.value) {
        this.updateEos()
      } else {
        this.presentAlertLatest()
      }
    } catch (e: any) {
      await loader.unsubscribe()
      this.errorService.handleError(e)
    }
  }

  private async presentAlertLatest() {
    const alert = await this.alertCtrl.create({
      header: 'Up to date!',
      message: 'You are on the latest version of StartOS.',
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
        description: 'Back up StartOS and service data',
        icon: 'duplicate-outline',
        action: () =>
          this.navCtrl.navigateForward(['backup'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Restore From Backup',
        description: 'Restore one or more services from backup',
        icon: 'color-wand-outline',
        action: () =>
          this.navCtrl.navigateForward(['restore'], { relativeTo: this.route }),
        detail: true,
        disabled$: this.eosService.updatingOrBackingUp$,
      },
    ],
    Manage: [
      {
        title: 'Software Update',
        description: 'Get the latest version of StartOS',
        icon: 'cloud-download-outline',
        action: () =>
          this.eosService.updateAvailable$.getValue()
            ? this.updateEos()
            : this.checkForEosUpdate(),
        detail: false,
        disabled$: this.eosService.updatingOrBackingUp$,
      },
      {
        title: 'Browser Tab Title',
        description: `Customize the display name of your browser tab`,
        icon: 'pricetag-outline',
        action: () => this.setBrowserTab(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Root CA',
        description: `Download and trust your server's Root Certificate Authority`,
        icon: 'ribbon-outline',
        action: () =>
          this.navCtrl.navigateForward(['root-ca'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'SSH',
        description:
          'Manage your SSH keys to access your server from the command line',
        icon: 'terminal-outline',
        action: () =>
          this.navCtrl.navigateForward(['ssh'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'WiFi',
        description: 'WiFi is deprecated. Click to learn more.',
        icon: 'wifi',
        action: () =>
          this.navCtrl.navigateForward(['wifi'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Sideload a Service',
        description: `Manually install a service`,
        icon: 'push-outline',
        action: () =>
          this.navCtrl.navigateForward(['sideload'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Change Master Password',
        description: `Change your StartOS master password`,
        icon: 'key-outline',
        action: () => this.presentAlertResetPassword(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Reset Tor',
        description: 'May help resolve Tor connectivity issues.',
        icon: 'reload-circle-outline',
        action: () => this.presentAlertResetTor(),
        detail: false,
        disabled$: of(false),
      },
    ],
    Insights: [
      {
        title: 'About',
        description: 'Basic information about your server',
        icon: 'information-circle-outline',
        action: () =>
          this.navCtrl.navigateForward(['specs'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Monitor',
        description: 'CPU, disk, memory, and other useful metrics',
        icon: 'pulse',
        action: () =>
          this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
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
        disabled$: of(false),
      },
      {
        title: 'OS Logs',
        description: 'Raw, unfiltered operating system logs',
        icon: 'receipt-outline',
        action: () =>
          this.navCtrl.navigateForward(['logs'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
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
        disabled$: of(false),
      },
      {
        title: 'Tor Logs',
        description: 'Diagnostic log stream for the Tor daemon on StartOS',
        icon: 'receipt-outline',
        action: () =>
          this.navCtrl.navigateForward(['tor-logs'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
    ],
    Support: [
      {
        title: 'User Manual',
        description: 'Discover what StartOS can do',
        icon: 'map-outline',
        action: () =>
          this.windowRef.open(
            'https://docs.start9.com/0.3.5.x/user-manual',
            '_blank',
            'noreferrer',
          ),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Contact Support',
        description: 'Get help from the Start9 team and community',
        icon: 'chatbubbles-outline',
        action: () =>
          this.windowRef.open(
            'https://start9.com/contact',
            '_blank',
            'noreferrer',
          ),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Donate to Start9',
        description: `Support StartOS development`,
        icon: 'logo-bitcoin',
        action: () =>
          this.windowRef.open(
            'https://donate.start9.com',
            '_blank',
            'noreferrer',
          ),
        detail: true,
        disabled$: of(false),
      },
    ],
    Power: [
      {
        title: 'Log Out',
        description: '',
        icon: 'log-out-outline',
        action: () => this.presentAlertLogout(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Restart',
        description: '',
        icon: 'reload',
        action: () => this.presentAlertRestart(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Shutdown',
        description: '',
        icon: 'power',
        action: () => this.presentAlertShutdown(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Repair Disk',
        description: '',
        icon: 'medkit-outline',
        action: () => this.presentAlertRepairDisk(),
        detail: false,
        disabled$: of(false),
      },
    ],
  }

  private async addManageClick() {
    this.manageClicks++
    if (this.manageClicks === 5) {
      this.manageClicks = 0
      const newVal = this.ClientStorageService.toggleShowDevTools()
      const toast = await this.toastCtrl.create({
        header: newVal ? 'Dev tools unlocked' : 'Dev tools hidden',
        position: 'bottom',
        duration: 1000,
      })

      await toast.present()
    }
  }

  private addPowerClick() {
    this.powerClicks++
    if (this.powerClicks === 5) {
      this.powerClicks = 0
      this.ClientStorageService.toggleShowDiskRepair()
    }
  }

  asIsOrder() {
    return 0
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
  disabled$: Observable<boolean>
}

const passwordSpec = CB.Config.of({
  currentPassword: CB.Value.text({
    name: 'Current Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword1: CB.Value.text({
    name: 'New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword2: CB.Value.text({
    name: 'Retype New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
})

export type PasswordSpec = typeof passwordSpec.validator._TYPE
