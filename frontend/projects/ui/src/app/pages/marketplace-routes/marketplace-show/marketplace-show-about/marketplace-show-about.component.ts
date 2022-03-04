import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'

@Component({
  selector: 'marketplace-show-about',
  templateUrl: 'marketplace-show-about.component.html',
  styleUrls: ['marketplace-show-about.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowAboutComponent {
  @Input()
  pkg: MarketplacePkg
}
