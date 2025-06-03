import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplaceAdditionalItemComponent } from './additional-item.component'

@Component({
  selector: 'marketplace-additional-link',
  template: `
    <a [href]="url" target="_blank" rel="noreferrer">
      <marketplace-additional-item
        [label]="label"
        [icon]="icon"
        [data]="url"
      ></marketplace-additional-item>
    </a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, MarketplaceAdditionalItemComponent],
})
export class MarketplaceAdditionalLinkComponent {
  @Input({ required: true })
  label!: string

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  url!: string
}
