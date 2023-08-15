import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { AbstractMarketplaceService } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject, filter, shareReplay, switchMap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TuiDialogService } from '@taiga-ui/core'
import { MarketplaceSettingsPage } from '../marketplace-list/marketplace-settings/marketplace-settings.page'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'marketplace-show',
  templateUrl: './marketplace-show.page.html',
  styleUrls: ['./marketplace-show.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowPage {
  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
    private readonly marketplaceService: AbstractMarketplaceService,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
    readonly config: ConfigService,
  ) {}

  readonly pkgId = getPkgId(this.route)
  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined
  readonly marketplace = this.config.marketplace

  readonly loadVersion$ = new BehaviorSubject<string>('*')

  readonly localPkg$ = this.patch
    .watch$('package-data', this.pkgId)
    .pipe(filter(Boolean), shareReplay({ bufferSize: 1, refCount: true }))

  readonly pkg$ = this.loadVersion$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(this.pkgId, version, this.url),
    ),
  )

  async presentModalMarketplaceSettings() {
    this.dialogs
      .open<MarketplaceSettingsPage>(
        new PolymorpheusComponent(MarketplaceSettingsPage),
        {
          label: 'Change Registry',
        },
      )
      .subscribe()
  }
}
