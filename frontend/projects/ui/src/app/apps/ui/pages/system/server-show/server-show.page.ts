import { Component, Inject } from '@angular/core'
import { NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActivatedRoute } from '@angular/router'
import { PatchDB } from 'patch-db-client'
import { filter, Observable, of, switchMap, take } from 'rxjs'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { EOSService } from 'src/app/services/eos.service'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { OSUpdatePage } from './os-update/os-update.page'
import { getAllPackages } from 'src/app/util/get-package-data'
import { AuthService } from 'src/app/services/auth.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormPage } from '../../../modals/form/form.page'
import { Config } from '@start9labs/start-sdk/lib/config/builder/config'
import { Value } from '@start9labs/start-sdk/lib/config/builder/value'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { ConfigService } from 'src/app/services/config.service'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import { PROMPT } from 'src/app/apps/ui/modals/prompt/prompt.component'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { WINDOW } from '@ng-web-apis/common'
import { getServerInfo } from 'src/app/util/get-server-info'
import * as argon2 from '@start9labs/argon2'
import { ProxyService } from 'src/app/services/proxy.service'

@Component({
  selector: 'server-show',
  templateUrl: 'server-show.page.html',
  styleUrls: ['server-show.page.scss'],
})
export class ServerShowPage {
  manageClicks = 0
  powerClicks = 0

  readonly server$ = this.patch.watch$('server-info')
  readonly showUpdate$ = this.eosService.showUpdate$
  readonly showDiskRepair$ = this.clientStorageService.showDiskRepair$

  readonly isTorHttp = this.config.isTorHttp()

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly clientStorageService: ClientStorageService,
    private readonly authService: AuthService,
    private readonly alerts: TuiAlertService,
    private readonly config: ConfigService,
    private readonly formDialog: FormDialogService,
    private readonly proxyService: ProxyService,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  addClick(title: string) {
    switch (title) {
      case 'Security':
        this.addSecurityClick()
        break
      case 'Power':
        this.addPowerClick()
        break
      default:
        return
    }
  }

  private async setBrowserTab(): Promise<void> {
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
      .subscribe(name => this.setName(name || null))
  }

  private updateEos() {
    this.dialogs.open(new PolymorpheusComponent(OSUpdatePage)).subscribe()
  }

  private presentAlertResetPassword() {
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
      .pipe(filter(Boolean))
      .subscribe(() => this.presentModalResetPassword())
  }

  async presentModalResetPassword(): Promise<void> {
    this.formDialog.open(FormPage, {
      label: 'Change Master Password',
      data: {
        spec: await configBuilderToSpec(passwordSpec),
        buttons: [
          {
            text: 'Save',
            handler: (value: PasswordSpec) => this.resetPassword(value),
          },
        ],
      },
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
    const { 'password-hash': passwordHash } = await getServerInfo(this.patch)
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
      await this.api.resetPassword({
        'old-password': value.currentPassword,
        'new-password': value.newPassword1,
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

  private presentAlertLogout() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Are you sure you want to log out?',
          yes: 'Logout',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.logout())
  }

  private presentAlertRestart() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Restart',
        size: 's',
        data: {
          content:
            'Are you sure you want to restart your server? It can take several minutes to come back online.',
          yes: 'Restart',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.restart())
  }

  private presentAlertShutdown() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, You will need to physically unplug your server and plug it back in',
          yes: 'Shutdown',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.shutdown())
  }

  private async presentAlertSystemRebuild() {
    const localPkgs = await getAllPackages(this.patch)
    const minutes = Object.keys(localPkgs).length * 2

    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content: `This action will tear down all service containers and rebuild them from scratch. No data will be deleted. This action is useful if your system gets into a bad state, and it should only be performed if you are experiencing general performance or reliability issues. It may take up to ${minutes} minutes to complete. During this time, you will lose all connectivity to your server.`,
          yes: 'Rebuild',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.systemRebuild())
  }

  private presentAlertRepairDisk() {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content: `This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action.<p>If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem <i>will</i> be in an unrecoverable state. Please proceed with caution.</p>`,
          yes: 'Rebuild',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.systemRebuild())
  }

  async launchHttps() {
    const info = await getServerInfo(this.patch)
    this.windowRef.open(`https://${info.ui.torHostname}`, '_self')
  }

  private async setName(value: string | null): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.setDbValue<string | null>(['name'], value)
    } finally {
      loader.unsubscribe()
    }
  }

  // should wipe cache independent of actual BE logout
  private logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.authService.setUnverified()
  }

  private async restart() {
    const action = 'Restart'
    const loader = this.loader.open(`Beginning ${action}...`).subscribe()

    try {
      await this.api.restartServer({})
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
      await this.api.shutdownServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async systemRebuild() {
    const action = 'System Rebuild'
    const loader = this.loader.open(`Beginning ${action}...`).subscribe()

    try {
      await this.api.systemRebuild({})
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

      loader.unsubscribe()

      if (this.eosService.updateAvailable$.value) {
        this.updateEos()
      } else {
        this.presentAlertLatest()
      }
    } catch (e: any) {
      loader.unsubscribe()
      this.errorService.handleError(e)
    }
  }

  private presentAlertLatest() {
    this.dialogs
      .open('You are on the latest version of StartOS.', {
        label: 'Up to date!',
        size: 's',
      })
      .subscribe()
  }

  settings: ServerSettings = {
    General: [
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
        title: 'Email',
        description:
          'Connect to an external SMTP server to send yourself emails',
        icon: 'mail-outline',
        action: () =>
          this.navCtrl.navigateForward(['email'], { relativeTo: this.route }),
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
        title: 'Experimental Features',
        description: 'Try out new and potentially unstable new features',
        icon: 'flask-outline',
        action: () =>
          this.navCtrl.navigateForward(['experimental-features'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
    ],
    Network: [
      {
        title: 'Domains',
        description: 'Manage domains for clearnet connectivity',
        icon: 'globe-outline',
        action: () =>
          this.navCtrl.navigateForward(['domains'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Proxies',
        description: 'Manage proxies for inbound and outbound connections',
        icon: 'shuffle-outline',
        action: () =>
          this.navCtrl.navigateForward(['proxies'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Router Config',
        description: 'Connect or configure your router for clearnet',
        icon: 'radio-outline',
        action: () =>
          this.navCtrl.navigateForward(['router-config'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'WiFi',
        description: 'Add or remove WiFi networks',
        icon: 'wifi',
        action: () =>
          this.navCtrl.navigateForward(['wifi'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
    ],
    'User Interface': [
      {
        title: 'Browser Tab Title',
        description: `Customize the display name of your browser tab`,
        icon: 'pricetag-outline',
        action: () => this.setBrowserTab(),
        detail: false,
        disabled$: of(false),
      },
      {
        title: 'Web Addresses',
        description: 'View and manage web addresses for accessing this UI',
        icon: 'desktop-outline',
        action: () =>
          this.navCtrl.navigateForward(['interfaces', 'ui'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
    ],
    'Privacy and Security': [
      {
        title: 'Outbound Proxy',
        description: 'Proxy outbound traffic from the StartOS main process',
        icon: 'shield-outline',
        action: () => this.proxyService.presentModalSetOutboundProxy(),
        detail: false,
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
        title: 'Active Sessions',
        description: 'View and manage device access',
        icon: 'stopwatch-outline',
        action: () =>
          this.navCtrl.navigateForward(['sessions'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
    ],
    Logs: [
      {
        title: 'Activity Monitor',
        description: 'CPU, disk, memory, and other useful metrics',
        icon: 'pulse',
        action: () =>
          this.navCtrl.navigateForward(['metrics'], { relativeTo: this.route }),
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
        title: 'System Rebuild',
        description: '',
        icon: 'construct-outline',
        action: () => this.presentAlertSystemRebuild(),
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

  private addSecurityClick() {
    this.manageClicks++

    if (this.manageClicks === 5) {
      this.manageClicks = 0
      this.alerts
        .open(
          this.clientStorageService.toggleShowDevTools()
            ? 'Dev tools unlocked'
            : 'Dev tools hidden',
        )
        .subscribe()
    }
  }

  private addPowerClick() {
    this.powerClicks++
    if (this.powerClicks === 5) {
      this.powerClicks = 0
      this.clientStorageService.toggleShowDiskRepair()
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

export const passwordSpec = Config.of({
  currentPassword: Value.text({
    name: 'Current Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword1: Value.text({
    name: 'New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
  newPassword2: Value.text({
    name: 'Retype New Password',
    required: {
      default: null,
    },
    masked: true,
  }),
})

type PasswordSpec = typeof passwordSpec.validator._TYPE
