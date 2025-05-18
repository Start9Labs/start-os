import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'

const ICONS: Record<string, string> = {
  all: '@tui.layout-grid',
  bitcoin: '@tui.bitcoin',
  messaging: '@tui.message-circle',
  communications: '@tui.message-circle',
  data: '@tui.file-text',
  'developer tools': '@tui.table-split',
  featured: '@tui.star',
  lightning: '@tui.zap',
  media: '@tui.circle-play',
  networking: '@tui.globe',
  social: '@tui.users',
  ai: '@tui.cpu',
}

@Component({
  selector: 'marketplace-categories',
  templateUrl: 'categories.component.html',
  styleUrls: ['categories.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CategoriesComponent {
  @Input()
  categories?: Record<string, T.Category>

  @Input()
  category = ''

  @Output()
  readonly categoryChange = new EventEmitter<string>()

  readonly fallback: Record<string, T.Category> = {
    a: { name: '' },
    b: { name: '' },
    c: { name: '' },
    d: { name: '' },
    e: { name: '' },
  }

  switchCategory(category: string): void {
    this.category = category
    this.categoryChange.emit(category)
  }

  determineIcon(category: string): string {
    return ICONS[category.toLowerCase()] || '@tui.box'
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
