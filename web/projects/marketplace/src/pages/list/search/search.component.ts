import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  inject,
  Input,
  Output,
} from '@angular/core'
import { THEME } from '@start9labs/shared'

@Component({
  selector: 'marketplace-search',
  templateUrl: 'search.component.html',
  styleUrls: ['search.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SearchComponent {
  @Input()
  query?: string | null = ''

  @Output()
  readonly queryChange = new EventEmitter<string>()

  readonly theme$ = inject(THEME)

  onModelChange(query: string) {
    this.query = query
    this.queryChange.emit(query)
  }
}
