import { Injectable } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { WizardBaker } from '../components/install-wizard/prebaked-wizards'
import { OSWelcomePage } from '../modals/os-welcome/os-welcome.page'
import { ConfigService } from './config.service'
import { PatchDbService } from './patch-db/patch-db.service'
import { filter, take } from 'rxjs/operators'
import { isEmptyObject } from '../util/misc.util'
import { ApiService } from './api/embassy-api.service'
import { Subscription } from 'rxjs'
import { ServerConfigService } from './server-config.service'

@Injectable({
  providedIn: 'root',
})
export class StartupAlertsService {
  private checks: Check<any>[]

  constructor(
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbService,
    private readonly serverConfig: ServerConfigService,
  ) {
    const osWelcome: Check<boolean> = {
      name: 'osWelcome',
      shouldRun: () => this.shouldRunOsWelcome(),
      display: () => this.displayOsWelcome(),
    }
    const shareStats: Check<boolean> = {
      name: 'shareStats',
      shouldRun: () => this.shouldRunShareStats(),
      display: () => this.displayShareStats(),
    }
    this.checks = [osWelcome, shareStats]
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
      .subscribe(async () => {
        await this.checks
          .filter(c => !this.config.skipStartupAlerts && c.shouldRun())
          // returning true in the below block means to continue to next modal
          // returning false means to skip all subsequent modals
          .reduce(async (previousDisplay, c) => {
            const displayRes = await previousDisplay
            if (displayRes) return c.display()
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
}

type Check<T> = {
  // validates whether a check should run based on server properties
  shouldRun: () => boolean
  // display an alert based on the result of the check.
  // return false if subsequent modals should not be displayed
  display: () => Promise<boolean>
  // for logging purposes
  name: string
}
