import { Injectable } from '@angular/core'
import { AlertController, IonicSafeString, ModalController, NavController } from '@ionic/angular'
import { wizardModal } from '../components/install-wizard/install-wizard.component'
import { WizardBaker } from '../components/install-wizard/prebaked-wizards'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { displayEmver } from '../pipes/emver.pipe'
import { RR } from './api/api.types'
import { PatchDbService } from './patch-db/patch-db.service'
import { ConfigService } from './config.service'
import { Emver } from './emver.service'
import { MarketplaceService } from '../pages/marketplace-routes/marketplace.service'
import { MarketplaceApiService } from './api/marketplace/marketplace-api.service'

@Injectable({
  providedIn: 'root',
})
export class StartupAlertsService {
  private checks: Check<any>[]

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly navCtrl: NavController,
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly marketplaceService: MarketplaceService,
    private readonly marketplaceApi: MarketplaceApiService,
    private readonly emver: Emver,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbService,
  ) {
    const welcome: Check<boolean> = {
      name: 'welcome',
      shouldRun: () => this.shouldRunOsWelcome(),
      check: async () => true,
      display: () => this.displayOsWelcome(),
      hasRun: this.config.skipStartupAlerts,
    }
    const osUpdate: Check<RR.GetMarketplaceEOSRes | undefined> = {
      name: 'osUpdate',
      shouldRun: () => this.shouldRunOsUpdateCheck(),
      check: () => this.osUpdateCheck(),
      display: pkg => this.displayOsUpdateCheck(pkg),
      hasRun: this.config.skipStartupAlerts,
    }
    const apps: Check<boolean> = {
      name: 'apps',
      shouldRun: () => this.shouldRunAppsCheck(),
      check: () => this.appsCheck(),
      display: () => this.displayAppsCheck(),
      hasRun: this.config.skipStartupAlerts,
    }
    this.checks = [welcome, osUpdate, apps]
  }

  // This takes our three checks and filters down to those that should run.
  // Then, the reduce fires, quickly iterating through yielding a promise (previousDisplay) to the next element
  // Each promise fires more or less concurrently, so each c.check(server) is run concurrently
  // Then, since we await previousDisplay before c.display(res), each promise executing gets hung awaiting the display of the previous run
  async runChecks (): Promise<void> {
    await this.checks
      .filter(c => !c.hasRun && c.shouldRun())
      // returning true in the below block means to continue to next modal
      // returning false means to skip all subsequent modals
      .reduce(async (previousDisplay, c) => {
        let checkRes: any
        try {
          checkRes = await c.check()
          console.log('CHECK RES', checkRes)
        } catch (e) {
          console.error(`Exception in ${c.name} check:`, e)
          return true
        }
        c.hasRun = true
        const displayRes = await previousDisplay

        if (!checkRes) return true
        if (displayRes) return c.display(checkRes)
      }, Promise.resolve(true))
  }

  private shouldRunOsWelcome (): boolean {
    const data = this.patch.data
    return !data.ui['welcome-ack'] && data['server-info'].version === this.config.version
  }

  private shouldRunOsUpdateCheck (): boolean {
    return this.patch.data.ui['auto-check-updates']
  }

  private shouldRunAppsCheck (): boolean {
    return this.patch.data.ui['auto-check-updates']
  }

  private async osUpdateCheck (): Promise<RR.GetMarketplaceEOSRes | undefined> {
    const res = await this.marketplaceApi.getEos({ })

    if (this.emver.compare(this.patch.data['server-info'].version, res.version) === -1) {
      return res
    } else {
      return undefined
    }
  }

  private async appsCheck (): Promise<boolean> {
    const updates = await this.marketplaceService.getUpdates(this.patch.data['package-data'])
    return !!updates.length
  }

  private async displayOsWelcome (): Promise<boolean> {
    return new Promise(async resolve => {
      const modal = await this.modalCtrl.create({
        backdropDismiss: false,
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        componentProps: {
          version: this.patch.data['server-info'].version,
        },
      })

      await modal.present()
      modal.onWillDismiss().then(res => {
        return resolve(res.data)
      })
    })
  }

  private async displayOsUpdateCheck (eos: RR.GetMarketplaceEOSRes): Promise<boolean> {
    const { update } = await this.presentAlertNewOS(eos.version)
    if (update) {
      const { cancelled } = await wizardModal(
        this.modalCtrl,
        this.wizardBaker.updateOS({
          version: eos.version,
          headline: eos.headline,
          releaseNotes: eos['release-notes'],
        }),
      )
      if (cancelled) return true
      return false
    }
    return true
  }

  private async displayAppsCheck (): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        backdropDismiss: true,
        header: 'Updates Available!',
        message: new IonicSafeString(
          `<div style="display: flex; flex-direction: column; justify-content: space-around; min-height: 100px">
            <div>New service updates are available in the Marketplace.</div>
            <div style="font-size:x-small">You can disable these checks in your Embassy Config</div>
          </div>
          `,
        ),
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => resolve(true),
          },
          {
            text: 'View in Marketplace',
            handler: () => {
              return this.navCtrl.navigateForward('/marketplace').then(() => resolve(false))
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
        message: new IonicSafeString(
          `<div style="display: flex; flex-direction: column; justify-content: space-around; min-height: 100px">
            <div>Update EmbassyOS to version ${displayEmver(versionLatest)}?</div>
            <div style="font-size:x-small">You can disable these checks in your Embassy Config</div>
          </div>
          `,
        ),
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
  shouldRun: () => boolean
  // executes a check, often requiring api call. It should return a false-y value if there should be no display.
  check: () => Promise<T>
  // display an alert based on the result of the check.
  // return false if subsequent modals should not be displayed
  display: (a: T) => Promise<boolean>
  // tracks if this check has run in this app instance.
  hasRun: boolean
  // for logging purposes
  name: string
}