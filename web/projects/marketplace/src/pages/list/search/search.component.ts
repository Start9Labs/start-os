import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiInput } from '@taiga-ui/core'

@Component({
  selector: 'marketplace-search',
  templateUrl: 'search.component.html',
  styleUrls: ['search.component.scss'],
  imports: [FormsModule, TuiInput],
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
