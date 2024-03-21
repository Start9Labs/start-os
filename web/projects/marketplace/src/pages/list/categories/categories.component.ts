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
  categories?: string[]

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
        return 'tuiIconGridLarge'
      case 'bitcoin':
        // @TODO need bitcoin icon
        return 'tuiIconBoldLarge'
      case 'messaging':
      case 'communications':
        return 'tuiIconMessageCircleLarge'
      case 'data':
        return 'tuiIconFileTextLarge'
      case 'developer tools':
        return 'tuiIconTableSplitLarge'
      case 'featured':
        return 'tuiIconStarLarge'
      case 'lightning':
        return 'tuiIconZapLarge'
      case 'media':
        return 'tuiIconPlayCircleLarge'
      case 'networking':
        return 'tuiIconGlobeLarge'
      case 'social':
        return 'tuiIconUsersLarge'
      case 'ai':
        return 'tuiIconCpuLarge'
      default:
        return 'tuiIconBoxLarge'
    }
  }
}
