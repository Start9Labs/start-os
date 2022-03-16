import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'

@Component({
  selector: 'marketplace-about',
  templateUrl: 'about.component.html',
  styleUrls: ['about.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AboutComponent {
  @Input()
  pkg: MarketplacePkg
}
