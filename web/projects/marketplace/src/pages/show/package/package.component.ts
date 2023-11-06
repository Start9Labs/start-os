import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { TuiDurationOptions, tuiFadeIn } from '@taiga-ui/core'
import { tuiPure } from '@taiga-ui/cdk'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiFadeIn],
})
export class PackageComponent {
  @Input()
  pkg!: MarketplacePkg

  readonly version$ = new BehaviorSubject('*')
  speed = 1000

  @tuiPure
  getAnimation(duration: number): TuiDurationOptions {
    return { value: '', params: { duration } }
  }
}
