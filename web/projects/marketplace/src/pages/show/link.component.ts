import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplaceItemComponent } from './item.component'
import { i18nKey } from '@start9labs/shared'

@Component({
  selector: 'marketplace-link',
  template: `
    <a [href]="url" target="_blank" rel="noreferrer">
      <marketplace-item [label]="label" [icon]="icon" [data]="url" />
    </a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [MarketplaceItemComponent],
})
export class MarketplaceLinkComponent {
  @Input({ required: true })
  label!: i18nKey

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  url!: string
}
