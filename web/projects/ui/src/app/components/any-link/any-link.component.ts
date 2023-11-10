import {
  Component,
  Input,
  ChangeDetectionStrategy,
  OnInit,
} from '@angular/core'

@Component({
  selector: 'any-link',
  templateUrl: './any-link.component.html',
  styleUrls: ['./any-link.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AnyLinkComponent implements OnInit {
  @Input() link!: string
  @Input() qp?: Record<string, string>
  externalLink = false

  ngOnInit() {
    try {
      const _ = new URL(this.link)
      this.externalLink = true
    } catch {
      this.externalLink = false
    }
  }
}
