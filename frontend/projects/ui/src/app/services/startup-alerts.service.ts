import { Injectable } from '@angular/core'
import {
  AlertController,
  IonicSafeString,
  ModalController,
  NavController,
} from '@ionic/angular'
import { wizardModal } from '../components/install-wizard/install-wizard.component'
import { WizardBaker } from '../components/install-wizard/prebaked-wizards'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { displayEmver } from '../pipes/emver.pipe'
import { RR } from './api/api.types'
import { ConfigService } from './config.service'
import { Emver } from './emver.service'
import { MarketplaceService } from '../pages/marketplace-routes/marketplace.service'
import { DataModel } from './patch-db/data-model'
import { PatchDbService } from './patch-db/patch-db.service'
import { filter, take } from 'rxjs/operators'
import { isEmptyObject } from '../util/misc.util'
import { ApiService } from './api/embassy-api.service'
import { Subscription } from 'rxjs'
import { ServerConfigService } from './server-config.service'
import { v4 } from 'uuid'

@Injectable({
  providedIn: 'root',
})
export class StartupAlertsService {
  private checks: Check<any>[]

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly navCtrl: NavController,
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly marketplaceService: MarketplaceService,
    private readonly api: ApiService,
    private readonly emver: Emver,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbService,
    private readonly serverConfig: ServerConfigService,
  ) {
    const osWelcome: Check<boolean> = {
      name: 'osWelcome',
      shouldRun: () => this.shouldRunOsWelcome(),
      check: async () => true,
      display: () => this.displayOsWelcome(),
    }
    const shareStats: Check<boolean> = {
      name: 'shareStats',
      shouldRun: () => this.shouldRunShareStats(),
      check: async () => true,
      display: () => this.displayShareStats(),
    }
    const osUpdate: Check<RR.GetMarketplaceEOSRes | undefined> = {
      name: 'osUpdate',
      shouldRun: () => this.shouldRunOsUpdateCheck(),
      check: () => this.osUpdateCheck(),
      display: pkg => this.displayOsUpdateCheck(pkg),
    }
    this.checks = [osWelcome, shareStats, osUpdate]
  }

  // This takes our three checks and filters down to those that should run.
  // Then, the reduce fires, quickly iterating through yielding a promise (previousDisplay) to the next element
  // Each promise fires more or less concurrently, so each c.check(server) is run concurrently
  // Then, since we await previousDisplay before c.display(res), each promise executing gets hung awaiting the display of the previous run
  runChecks(): Subscription {
    return this.patch
      .watch$()
      .pipe(
        filter(data => !isEmptyObject(data)),
        take(1),
      )
      .subscribe(async data => {
        if (!data.ui.marketplace) {
          const uuid = v4()
          const value = {
            'selected-id': uuid,
            options: {
              [uuid]: {
                url: 'marketplaceurl.com',
                name: 'Start9',
              },
            },
          }
          await this.api.setDbValue({ pointer: 'marketplace', value })
        }
        await this.checks
          .filter(c => !this.config.skipStartupAlerts && c.shouldRun())
          // returning true in the below block means to continue to next modal
          // returning false means to skip all subsequent modals
          .reduce(async (previousDisplay, c) => {
            let checkRes: any
            try {
              checkRes = await c.check()
            } catch (e) {
              console.error(`Exception in ${c.name} check:`, e)
              return true
            }

            const displayRes = await previousDisplay

            if (!checkRes) return true
            if (displayRes) return c.display(checkRes)
          }, Promise.resolve(true))
      })
  }

  // ** should run **

  private shouldRunOsWelcome(): boolean {
    return this.patch.getData().ui['ack-welcome'] !== this.config.version
  }
  private shouldRunShareStats(): boolean {
    return !this.patch.getData().ui['ack-share-stats']
  }

  private shouldRunOsUpdateCheck(): boolean {
    return this.patch.getData().ui['auto-check-updates']
  }

  // ** check **

  private async osUpdateCheck(): Promise<RR.GetMarketplaceEOSRes | undefined> {
    const res = await this.api.getEos({
      'eos-version-compat':
        this.patch.getData()['server-info']['eos-version-compat'],
    })

    if (this.emver.compare(this.config.version, res.version) === -1) {
      return res
    } else {
      return undefined
    }
  }

  // ** display **

  private async displayOsWelcome(): Promise<boolean> {
    return new Promise(async resolve => {
      const modal = await this.modalCtrl.create({
        component: OSWelcomePage,
        presentingElement: await this.modalCtrl.getTop(),
        componentProps: {
          version: this.config.version,
        },
      })
      modal.onWillDismiss().then(() => {
        this.api
          .setDbValue({ pointer: '/ack-welcome', value: this.config.version })
          .catch()
        return resolve(true)
      })
      await modal.present()
    })
  }

  private async displayShareStats(): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.serverConfig.presentAlert(
        'share-stats',
        this.patch.getData()['server-info']['share-stats'],
      )

      alert.onDidDismiss().then(() => {
        this.api
          .setDbValue({
            pointer: '/ack-share-stats',
            value: this.config.version,
          })
          .catch()
        return resolve(true)
      })
    })
  }

  private async displayOsUpdateCheck(
    eos: RR.GetMarketplaceEOSRes,
  ): Promise<boolean> {
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

  // more

  private async presentAlertNewOS(
    versionLatest: string,
  ): Promise<{ cancel?: true; update?: true }> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'New EmbassyOS Version!',
        message: new IonicSafeString(
          `<div style="display: flex; flex-direction: column; justify-content: space-around; min-height: 100px">
            <div>Update EmbassyOS to version ${displayEmver(
              versionLatest,
            )}?</div>
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
            cssClass: 'enter-click',
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
  // for logging purposes
  name: string
}
