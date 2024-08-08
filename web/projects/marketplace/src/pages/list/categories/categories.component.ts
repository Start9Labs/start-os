import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'marketplace-categories',
  templateUrl: 'categories.component.html',
  styleUrls: ['categories.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CategoriesComponent {
  @Input()
  categories!: Map<string, T.Category>

  @Input()
  category = ''

  @Output()
  readonly categoryChange = new EventEmitter<string>()

  switchCategory(category: string): void {
    this.category = category
    this.categoryChange.emit(category)
  }

  determineIcon(category: string): string {
    switch (category.toLowerCase()) {
      case 'all':
        return '@tui.layout-grid'
      case 'bitcoin':
        return '@tui.bitcoin'
      case 'messaging':
      case 'communications':
        return '@tui.message-circle'
      case 'data':
        return '@tui.file-text'
      case 'developer tools':
        return '@tui.table-split'
      case 'featured':
        return '@tui.star'
      case 'lightning':
        return '@tui.zap'
      case 'media':
        return '@tui.circle-play'
      case 'networking':
        return '@tui.globe'
      case 'social':
        return '@tui.users'
      case 'ai':
        return '@tui.cpu'
      default:
        return '@tui.box'
    }
  }
}
