import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { BehaviorSubject } from 'rxjs'
import { switchMap } from 'rxjs/operators'
import { TuiDurationOptions, tuiFadeIn } from '@taiga-ui/core'
import { tuiPure } from '@taiga-ui/cdk'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.less'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiFadeIn],
})
export class PackageComponent {
  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly pkgId = getPkgId(this.activatedRoute)
  readonly version$ = new BehaviorSubject('*')
  index = 0
  speed = 1000

  readonly pkg$ = this.version$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(this.pkgId, version),
    ),
  )

  constructor(private readonly activatedRoute: ActivatedRoute) {}

  @tuiPure
  getAnimation(duration: number): TuiDurationOptions {
    return { value: '', params: { duration } }
  }
}
