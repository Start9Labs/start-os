import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiIcon } from '@taiga-ui/core'

@Component({
  selector: 'marketplace-search',
  templateUrl: 'search.component.html',
  styleUrls: ['search.component.scss'],
  imports: [FormsModule, CommonModule, TuiIcon],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SearchComponent {
  @Input()
  query?: string | null = ''

  @Output()
  readonly queryChange = new EventEmitter<string>()

  onModelChange(query: string) {
    this.query = query
    this.queryChange.emit(query)
  }
}
