import {
  Component,
  Input,
  ChangeDetectionStrategy,
  OnChanges,
} from '@angular/core'

@Component({
  selector: 'any-link',
  templateUrl: './any-link.component.html',
  styleUrls: ['./any-link.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AnyLinkComponent implements OnChanges {
  @Input() link!: string
  externalLink: boolean = false

  ngOnChanges() {
    try {
      const _ = new URL(this.link)
      this.externalLink = true
    } catch {
      this.externalLink = false
    }
  }
}
