import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnDestroy,
  signal,
} from '@angular/core'
import { DocsLinkDirective, i18nPipe } from '@start9labs/shared'
import { TuiAppearance, TuiButton, TuiIcon, TuiPopup } from '@taiga-ui/core'
import { TuiDrawer, TuiSkeleton } from '@taiga-ui/kit'
import { Subject, takeUntil } from 'rxjs'
import { CategoriesComponent } from '../../pages/list/categories/categories.component'
import { SearchComponent } from '../../pages/list/search/search.component'
import { AbstractCategoryService } from '../../services/category.service'
import { StoreDataWithUrl } from '../../types'
import { StoreIconComponent } from '../store-icon.component'

@Component({
  selector: 'menu',
  templateUrl: './menu.component.html',
  styleUrls: ['./menu.component.scss'],
  imports: [
    CommonModule,
    SearchComponent,
    CategoriesComponent,
    TuiButton,
    StoreIconComponent,
    TuiAppearance,
    TuiIcon,
    TuiSkeleton,
    TuiDrawer,
    TuiPopup,
    i18nPipe,
    DocsLinkDirective,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent implements OnDestroy {
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
