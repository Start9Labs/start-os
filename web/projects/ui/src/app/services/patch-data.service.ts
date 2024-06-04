import { Inject, Injectable } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { Observable } from 'rxjs'
import { filter, map, share, switchMap, take, tap } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { EOSService } from 'src/app/services/eos.service'
import { OSWelcomePage } from 'src/app/modals/os-welcome/os-welcome.page'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { ConnectionService } from 'src/app/services/connection.service'
import { LocalStorageBootstrap } from './patch-db/local-storage-bootstrap'

// Get data from PatchDb after is starts and act upon it
@Injectable({
  providedIn: 'root',
})
export class PatchDataService extends Observable<void> {
  private readonly stream$ = this.connectionService.connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$()),
    map((cache, index) => {
      this.bootstrapper.update(cache)

      if (index === 0) {
        // check for updates to StartOS and services
        this.checkForUpdates()
        // show eos welcome message
        this.showEosWelcome(cache.ui.ackWelcome)
      }
    }),
    share(),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly config: ConfigService,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly connectionService: ConnectionService,
    private readonly bootstrapper: LocalStorageBootstrap,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private checkForUpdates(): void {
    this.eosService.loadEos()
    this.marketplaceService.getMarketplace$().pipe(take(1)).subscribe()
  }

  private async showEosWelcome(ackVersion: string): Promise<void> {
    if (this.config.skipStartupAlerts || ackVersion === this.config.version) {
      return
    }

    const modal = await this.modalCtrl.create({
      component: OSWelcomePage,
      presentingElement: await this.modalCtrl.getTop(),
      backdropDismiss: false,
    })
    modal.onWillDismiss().then(() => {
      this.embassyApi
        .setDbValue<string>(['ackWelcome'], this.config.version)
        .catch()
    })

    await modal.present()
  }
}
