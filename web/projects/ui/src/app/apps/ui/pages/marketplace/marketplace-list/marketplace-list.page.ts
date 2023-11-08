import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  AbstractCategoryService,
  AbstractMarketplaceService,
} from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { ConfigService } from 'src/app/services/config.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import {
  animate,
  query,
  stagger,
  style,
  transition,
  trigger,
} from '@angular/animations'
import { CategoryService } from 'src/app/services/category.service'
import { MarketplaceSettingsPage } from './marketplace-settings/marketplace-settings.page'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { ActivatedRoute } from '@angular/router'
import { DOCUMENT } from '@angular/common'

@Component({
  selector: 'marketplace-list',
  templateUrl: 'marketplace-list.page.html',
  styleUrls: ['./marketplace-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [
    trigger('itemAnimation', [
      transition('* => *', [
        query(
          ':enter',
          [
            style({ opacity: 0, transform: 'translateY(-20px)' }),
            stagger('100ms', [
              animate(
                '300ms ease-in-out',
                style({ opacity: 1, transform: 'none' }),
              ),
            ]),
          ],
          { optional: true },
        ),
      ]),
    ]),
  ],
})
export class MarketplaceListPage {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    @Inject(AbstractCategoryService)
    private readonly categoryService: CategoryService,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
    readonly config: ConfigService,
    private readonly route: ActivatedRoute,
    @Inject(DOCUMENT) private readonly document: Document,
  ) {}

  readonly packages$ = this.marketplaceService
    .getSelectedStore$()
    .pipe(map(({ packages }) => packages))
  readonly localPkgs$ = this.patch.watch$('package-data')
  readonly category$ = this.categoryService.getCategory$()
  readonly query$ = this.categoryService.getQuery$()
  readonly details$ = this.marketplaceService.getSelectedHost$()
  readonly marketplace = this.config.marketplace
  readonly pkgId = this.route.snapshot.queryParamMap.get('id') || undefined

  ngAfterViewInit() {
    if (this.pkgId) {
      this.document.getElementById(this.pkgId)?.click()
    }
  }

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
