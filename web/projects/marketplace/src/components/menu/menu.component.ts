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
import { MarketplaceConfig } from '@start9labs/shared'

@Component({
  selector: 'menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent implements OnDestroy {
  @Input({ required: true })
  iconConfig!: MarketplaceConfig

  private destroy$ = new Subject<void>()
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  private readonly categoryService = inject(AbstractCategoryService)
  readonly store$ = this.marketplaceService.getSelectedRegistryWithCategories$()
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
  }

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
    this.categoryService.resetQuery()
    this.categoryService.changeCategory(category)
    this.categoryService.handleNavigation()
  }

  onQueryChange(query: string): void {
    this.query = query
    this.categoryService.setQuery(query)
  }

  toggleMenu(open: boolean): void {
    this.open = open
  }

  ngOnDestroy(): void {
    this.destroy$.next()
    this.destroy$.complete()
  }
}
