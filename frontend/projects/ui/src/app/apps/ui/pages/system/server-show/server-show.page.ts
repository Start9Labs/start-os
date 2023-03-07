import { DOCUMENT } from '@angular/common'
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
import { ConfigService } from 'src/app/services/config.service'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import { PROMPT } from 'src/app/apps/ui/modals/prompt/prompt.component'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { TUI_PROMPT } from '@taiga-ui/kit'

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

  readonly secure = this.config.isSecure()

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly navCtrl: NavController,
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly clientStorageService: ClientStorageService,
    private readonly authService: AuthService,
    private readonly alerts: TuiAlertService,
    private readonly config: ConfigService,
    @Inject(DOCUMENT) private readonly document: Document,
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

  private async setName(value: string | null): Promise<void> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.embassyApi.setDbValue<string | null>(['name'], value)
    } finally {
      loader.unsubscribe()
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
      this.presentAlertInProgress(action, ` until ${action} completes.`)
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
      this.presentAlertInProgress(
        action,
        '.<br /><br /><b>You will need to physically power cycle the device to regain connectivity.</b>',
      )
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
      await this.embassyApi.systemRebuild({})
      this.presentAlertInProgress(action, ` until ${action} completes.`)
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

  private presentAlertInProgress(verb: string, message: string) {
    this.dialogs
      .open(
        `Stopping all services gracefully. This can take a while.<br /><br />If you have a speaker, your server will <b>♫ play a melody ♫</b> before shutting down. Your server will then become unreachable${message}`,
        {
          label: `${verb} In Progress...`,
          size: 's',
        },
      )
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
        title: 'Browser Tab Title',
        description: `Customize the display name of your browser tab`,
        icon: 'pricetag-outline',
        action: () => this.setBrowserTab(),
        detail: false,
        disabled$: of(false),
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
        title: 'StartOS Web Interface',
        description: 'Addresses for accessing this StartOS web interface',
        icon: 'desktop-outline',
        action: () =>
          this.navCtrl.navigateForward(['addresses'], {
            relativeTo: this.route,
          }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Domains',
        description:
          'Add domains to your server to enable clearnet connections',
        icon: 'globe-outline',
        action: () =>
          this.navCtrl.navigateForward(['domains'], { relativeTo: this.route }),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Port Forwards',
        description:
          'A list of ports that should be forwarded through your router',
        icon: 'trail-sign-outline',
        action: () =>
          this.navCtrl.navigateForward(['port-forwards'], {
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
    Security: [
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
        title: 'System Resources',
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
          window.open(
            'https://docs.start9.com/latest/user-manual',
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
          window.open('https://start9.com/contact', '_blank', 'noreferrer'),
        detail: true,
        disabled$: of(false),
      },
      {
        title: 'Donate to Start9',
        description: `Support StartOS development`,
        icon: 'logo-bitcoin',
        action: () =>
          this.document.defaultView?.open(
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
