import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnDestroy,
} from '@angular/core'
import { MarketplaceConfig } from '@start9labs/shared'
import { Subject, takeUntil } from 'rxjs'
import { AbstractCategoryService } from '../../services/category.service'
import { StoreData } from '../../types'

@Component({
  selector: 'menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent implements OnDestroy {
  @Input({ required: true })
  iconConfig!: MarketplaceConfig

  @Input({ required: true })
  registry: (StoreData & { url?: string }) | null = null

  private destroy$ = new Subject<void>()
  private readonly categoryService = inject(AbstractCategoryService)
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
