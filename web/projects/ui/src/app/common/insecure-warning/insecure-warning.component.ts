import { DOCUMENT } from '@angular/common'
import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'

@Component({
  selector: 'insecure-warning',
  templateUrl: './insecure-warning.component.html',
  styleUrls: ['./insecure-warning.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class InsecureWarningComponent {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  launchHttps() {
    this.document.defaultView?.open(
      this.document.location.href.replace('http', 'https'),
    )
  }
}
