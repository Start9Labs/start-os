import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnDestroy,
} from '@angular/core'
import { combineLatest, map, Subject, takeUntil } from 'rxjs'
import { StoreIdentity } from '../../types'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { AbstractCategoryService } from '../../services/category.service'
import { Router } from '@angular/router'
import { MarketplaceConfig } from '@start9labs/shared'

@Component({
  selector: 'sidebar',
  templateUrl: './sidebar.component.html',
  styleUrls: ['./sidebar.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SidebarComponent implements OnDestroy {
  @Input({ required: true })
  iconConfig!: MarketplaceConfig

  constructor(private readonly router: Router) {}

  private destroy$ = new Subject<void>()
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  private readonly categoryService = inject(AbstractCategoryService)
  readonly store$ = this.marketplaceService.getSelectedStoreWithCategories$()
  readonly alt$ = combineLatest([
    this.marketplaceService.getKnownHosts$(),
    this.marketplaceService.getSelectedHost$(),
  ]).pipe(
    map(([stores, selected]) =>
      stores.filter(({ url }) => url != selected.url),
    ),
  )
  private hosts?: StoreIdentity[]
  category = ''
  query = ''
  open = false

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
    this.router.navigate(['/marketplace'], { replaceUrl: true })
  }

  onQueryChange(query: string): void {
    this.query = query
    this.categoryService.setQuery(query)
    this.router.navigate(['/marketplace'], { replaceUrl: true })
  }

  toggleMenu(open: boolean): void {
    this.open = open
  }

  ngOnDestroy(): void {
    this.destroy$.next()
    this.destroy$.complete()
  }
}
