import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
  OnDestroy,
} from '@angular/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { RegistrySettingsComponent } from '../registry-settings/registry-settings.component'
import { TuiDialogService } from '@taiga-ui/core'
import { combineLatest, map, Subject, takeUntil, tap } from 'rxjs'
import { StoreIdentity } from '../../../src/types'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { AbstractCategoryService } from '../../services/category.service'

@Component({
  selector: 'marketplace-header',
  templateUrl: './header.component.html',
  styleUrls: ['./header.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HeaderComponent implements OnDestroy {
  constructor(
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  private destroy$ = new Subject<void>()
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  private readonly categoryService = inject(AbstractCategoryService)
  readonly store$ = this.marketplaceService.getSelectedStoreWithAllCategories$()
  readonly alt$ = combineLatest([
    this.marketplaceService.getKnownHosts$(),
    this.marketplaceService.getSelectedHost$(),
  ]).pipe(
    map(([stores, selected]) =>
      stores.filter(({ url }) => url != selected.url),
    ),
  )
  private hosts?: StoreIdentity[]
  category: string = ''
  query: string = ''
  open = false
  iconConfig = require('../../../../../config.json').ui.marketplace

  ngOnInit() {
    this.categoryService
      .getQuery$()
      .pipe(takeUntil(this.destroy$))
      .subscribe(val => {
        this.query = val
      })

    this.categoryService
      .getCategory$()
      .pipe(takeUntil(this.destroy$))
      .subscribe(val => {
        this.category = val
      })

    this.marketplaceService
      .getKnownHosts$()
      .pipe(takeUntil(this.destroy$))
      .subscribe(hosts => {
        this.hosts = hosts
      })
  }

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
    this.categoryService.resetQuery()
    this.categoryService.changeCategory(category)
  }

  onQueryChange(query: string): void {
    this.query = query
    this.categoryService.setQuery(query)
  }

  changeRegistry() {
    this.dialogs
      .open<StoreIdentity>(
        new PolymorpheusComponent(RegistrySettingsComponent),
        {
          label: 'Change Registry',
          data: this.hosts,
          dismissible: true,
        },
      )
      .subscribe({
        next: data => {
          // this.urlService.toggle((data as any).url);
        },
      })
  }

  toggleMenu(open: boolean): void {
    this.open = open
  }

  ngOnDestroy(): void {
    this.destroy$.next()
    this.destroy$.complete()
  }
}
