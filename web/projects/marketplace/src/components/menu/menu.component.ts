import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnDestroy,
  signal,
} from '@angular/core'
import { knownMarketplaceUrls } from '@start9labs/shared'
import { Subject, takeUntil } from 'rxjs'
import { AbstractCategoryService } from '../../services/category.service'
import { StoreDataWithUrl } from '../../types'

@Component({
  selector: 'menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent implements OnDestroy {
  @Input({ required: true })
  iconConfig!: typeof knownMarketplaceUrls

  @Input({ required: true })
  registry!: StoreDataWithUrl | null

  private destroy$ = new Subject<void>()
  private readonly categoryService = inject(AbstractCategoryService)
  category = ''
  query = ''
  readonly open = signal(false)

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
  }

  onQueryChange(query: string): void {
    this.query = query
    this.categoryService.setQuery(query)
  }

  ngOnDestroy(): void {
    this.destroy$.next()
    this.destroy$.complete()
  }
}
