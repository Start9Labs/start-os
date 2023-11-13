import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'marketplace-skeleton',
  templateUrl: 'skeleton.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SkeletonComponent {}
