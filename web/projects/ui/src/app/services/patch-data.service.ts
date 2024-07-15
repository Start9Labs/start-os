import { Inject, Injectable } from '@angular/core'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import { filter, share, switchMap, take, tap, Observable } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { EOSService } from 'src/app/services/eos.service'
import { WelcomeComponent } from 'src/app/components/welcome.component'
import { ConfigService } from 'src/app/services/config.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'

// Get data from PatchDb after is starts and act upon it
@Injectable({
  providedIn: 'root',
})
export class PatchDataService extends Observable<DataModel> {
  private readonly stream$ = this.connectionService.connected$.pipe(
    filter(Boolean),
    switchMap(() => this.patch.watch$()),
    take(1),
    tap(({ ui }) => {
      // check for updates to eOS and services
      this.checkForUpdates()
      // show eos welcome message
      this.showEosWelcome(ui.ackWelcome)
    }),
    share(),
  )

  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly eosService: EOSService,
    private readonly config: ConfigService,
    private readonly dialogs: TuiDialogService,
    private readonly embassyApi: ApiService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly connectionService: ConnectionService,
  ) {
    super(subscriber => this.stream$.subscribe(subscriber))
  }

  private checkForUpdates(): void {
    this.eosService.loadEos()
    this.marketplaceService.getMarketplace$().pipe(take(1)).subscribe()
  }

  private showEosWelcome(ackVersion: string) {
    if (this.config.skipStartupAlerts || ackVersion === this.config.version) {
      return
    }

    this.dialogs
      .open(new PolymorpheusComponent(WelcomeComponent), {
        label: 'Release Notes',
      })
      .subscribe({
        complete: () => {
          this.embassyApi
            .setDbValue<string>(['ackWelcome'], this.config.version)
            .catch()
        },
      })
  }
}
