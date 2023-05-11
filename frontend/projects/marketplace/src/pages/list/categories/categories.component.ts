import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'

@Component({
  selector: 'marketplace-categories',
  templateUrl: 'categories.component.html',
  styleUrls: ['categories.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CategoriesComponent {
  @Input()
  categories: readonly string[] = []

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
        return 'apps-outline'
      case 'bitcoin':
        return 'logo-bitcoin'
      case 'communications':
        return 'chatbubbles-outline'
      case 'data':
        return 'document'
      case 'developer tools':
        return 'code-slash-outline'
      case 'featured':
        return 'star-outline'
      case 'lightning':
        return 'flash-outline'
      case 'media':
        return 'play-outline'
      case 'networking':
        return 'globe-outline'
      case 'social':
        return 'people-outline'
      default:
        return 'cube-outline'
    }
  }
}
