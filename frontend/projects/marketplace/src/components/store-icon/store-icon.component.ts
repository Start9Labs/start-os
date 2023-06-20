import {
  ChangeDetectionStrategy,
  Component,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { MarketplaceConfig, sameUrl } from '@start9labs/shared'

@Component({
  selector: 'store-icon',
  templateUrl: './store-icon.component.html',
  styleUrls: ['./store-icon.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class StoreIconComponent {
  @Input()
  url = ''
  @Input()
  size?: string
  @Input()
  marketplace!: MarketplaceConfig

  get icon() {
    const { start9, community } = this.marketplace

    if (sameUrl(this.url, start9)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.url, community)) {
      return 'assets/img/community-store.png'
    }
    return null
  }
}
