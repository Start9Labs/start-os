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
  host: {
    class: 'hidden-scrollbar ion-text-center',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CategoriesComponent {
  @Input()
  categories!: Set<string>

  @Input()
  category!: string

  @Input()
  updatesAvailable!: number

  @Output()
  readonly categoryChange = new EventEmitter<string>()

  switchCategory(category: string): void {
    this.category = category
    this.categoryChange.emit(category)
  }
}
