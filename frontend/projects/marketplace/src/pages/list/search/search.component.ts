import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'

@Component({
  selector: 'marketplace-search',
  templateUrl: 'search.component.html',
  styleUrls: ['search.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SearchComponent {
  @Input()
  query = ''

  @Output()
  readonly queryChange = new EventEmitter<string>()

  onModelChange(query: string) {
    this.query = query
    this.queryChange.emit(query)
  }
}
