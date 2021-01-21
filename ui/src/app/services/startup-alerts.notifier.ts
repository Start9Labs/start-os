import { Injectable } from '@angular/core'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { S9Server } from '../models/server-model'
import { displayEmver } from '../pipes/emver.pipe'
import { ApiService } from './api/api.service'
import { ConfigService } from './config.service'
import { Emver } from './emver.service'
import { LoaderService } from './loader.service'
import { OsUpdateService } from './os-update.service'

@Injectable({ providedIn: 'root' })
export class StartupAlertsNotifier {
  constructor (
    private readonly alertCtrl: AlertController,
    private readonly navCtrl: NavController,
    private readonly loader: LoaderService,
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly emver: Emver,
    private readonly osUpdateService: OsUpdateService,
  ) { }

  displayedWelcomeMessage = false
  checkedOSForUpdates = false
  checkedAppsForUpdates = false

  // So. This takes our three checks and filters down to those that should run.
  // Then, the reduce fires, quickly iterating through yielding a promise (acc) to the next element
  // Each promise fires more or less concurrently, so each c.check(server) is run concurrently
  // Then, since we await acc before c.display(res), each promise executing gets hung awaiting the display of the previous run
  async runChecks (server: Readonly<S9Server>): Promise<void> {
    await this.checks
      .filter(c => c.shouldRun(server) && !c.hasRun)
      .reduce(async (previousDisplay, c) => {
        let checkRes
        try {
          checkRes = await c.check(server)
        } catch (e) {
          return console.error(`Exception in ${c.name} check:`, e)
        }
        c.hasRun = true
        if (!checkRes) return
        const displayRes = await previousDisplay
        if (c.shouldRun(server) && !!displayRes) return c.display(checkRes)
      }, Promise.resolve(true))
  }

  welcome: Check<S9Server> = {
    name: 'welcome',
    shouldRun: s => this.shouldRunOsWelcome(s),
    check: async s => s,
    display: s => this.displayOsWelcome(s),
    hasRun: false,
  }
  osUpdate: Check<string | undefined> = {
    name: 'osUpdate',
    shouldRun: s => this.shouldRunOsUpdateCheck(s),
    check: s => this.osUpdateCheck(s),
    display: vl => this.displayOsUpdateCheck(vl),
    hasRun: false,
  }
  apps: Check<boolean> = {
    name: 'apps',
    shouldRun: s => this.shouldRunAppsCheck(s),
    check: () => this.appsCheck(),
    display: () => this.displayAppsCheck(),
    hasRun: false,
  }

  checks: Check<any>[] = [this.welcome, this.apps, this.osUpdate]

  private shouldRunOsWelcome (s: S9Server): boolean {
    return !s.welcomeAck && s.versionInstalled === this.config.version
  }

  private shouldRunAppsCheck (server: S9Server): boolean {
    return server.autoCheckUpdates
  }

  private shouldRunOsUpdateCheck (server: S9Server): boolean {
    return server.autoCheckUpdates
  }


  private async osUpdateCheck (s: Readonly<S9Server>): Promise<string | undefined> {
    const { versionLatest } = await this.apiService.getVersionLatest()
    return this.osUpdateService.updateIsAvailable(s.versionInstalled, versionLatest) ? versionLatest : undefined
  }

  private async appsCheck (): Promise<boolean> {
    const availableApps = await this.apiService.getAvailableApps()
    return !!availableApps.find(app => this.emver.compare(app.versionInstalled, app.versionLatest) === -1)
  }

  private async displayOsWelcome (s: Readonly<S9Server>): Promise<boolean> {
    return new Promise(async resolve => {
      const modal = await this.modalCtrl.create({
        backdropDismiss: false,
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        componentProps: {
          version: s.versionInstalled,
        },
      })

      await modal.present()
      modal.onWillDismiss().then(res => {
        s = Object.assign(s, res.data)
        return resolve(true)
      })
    })
  }

  private async displayOsUpdateCheck (versionLatest: string | undefined): Promise<boolean> {
    const { update } = await this.presentAlertNewOS(versionLatest)
    if (update) {
      await this.loader.displayDuringP(
        this.osUpdateService.updateEmbassyOS(versionLatest),
        ).catch(e => alert(e))
      return false
    }
    return true
  }

  private async displayAppsCheck (): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        backdropDismiss: true,
        header: 'Updates Available!',
        message: 'New service updates are available in the Marketplace.',
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => resolve(true),
          },
          {
            text: 'View in Marketplace',
            handler: () => {
              return this.navCtrl.navigateForward('/services/marketplace').then(() => resolve(false))
            },
          },
        ],
      })

      await alert.present()
    })
  }

  private async presentAlertNewOS (versionLatest: string): Promise<{ cancel?: true, update?: true }> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        backdropDismiss: true,
        header: 'New EmbassyOS Version!',
        message: `Update EmbassyOS to version ${displayEmver(versionLatest)}?`,
        buttons: [
          {
            text: 'Not now',
            role: 'cancel',
            handler: () => resolve({ cancel: true }),
          },
          {
            text: 'Update',
            handler: () => resolve({ update: true }),
          },
        ],
      })
      await alert.present()
    })
  }
}

type Check<T> = {
  // validates whether a check should run based on server properties
  shouldRun: (s: S9Server) => boolean
  // executes a check, often requiring api call. It should return a false-y value if there should be no display.
  check: (s: S9Server) => Promise<T>
  // display an alert based on the result of the check.
  // return false if subsequent modals should be cancelled
  display: (a: T) => Promise<boolean>
  // tracks if this check has run in this app instance.
  hasRun: boolean
  // for logging purposes
  name: string
}