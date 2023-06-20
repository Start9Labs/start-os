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
  url: string = ''
  @Input()
  size?: string
  @Input()
  marketplace?: MarketplaceConfig
}

@Pipe({
  name: 'getIcon',
})
export class GetIconPipe implements PipeTransform {
  transform(url: string, marketplace?: MarketplaceConfig): string | null {
    if (marketplace) {
      const { start9, community } = marketplace

      if (sameUrl(url, start9)) {
        return 'assets/img/icon_transparent.png'
      } else if (sameUrl(url, community)) {
        return 'assets/img/community-store.png'
      }
      return null
    }
    return null
  }
}
