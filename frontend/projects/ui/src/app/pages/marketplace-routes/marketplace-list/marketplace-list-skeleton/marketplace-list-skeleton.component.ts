import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'marketplace-list-skeleton',
  templateUrl: 'marketplace-list-skeleton.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListSkeletonComponent {}
