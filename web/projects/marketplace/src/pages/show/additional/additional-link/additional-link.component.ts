import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { Url } from '@start9labs/shared'

@Component({
  selector: 'marketplace-additional-link',
  templateUrl: 'additional-link.component.html',
  styleUrls: ['additional-link.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalLinkComponent {
  @Input({ required: true })
  url!: Url

  @Input({ required: true })
  label!: string
}
