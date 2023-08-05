import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { BehaviorSubject } from 'rxjs'
import { switchMap } from 'rxjs/operators'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiDurationOptions,
  tuiFadeIn,
} from '@taiga-ui/core'
import { tuiPure } from '@taiga-ui/cdk'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'
import { isPlatform } from '@ionic/angular'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'marketplace-package',
  templateUrl: './package.component.html',
  styleUrls: ['./package.component.less'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiFadeIn],
})
export class PackageComponent {
  constructor(
    private readonly activatedRoute: ActivatedRoute,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly pkgId = getPkgId(this.activatedRoute)
  readonly version$ = new BehaviorSubject('*')
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')
  index = 0
  speed = 1000

  readonly pkg$ = this.version$.pipe(
    switchMap(version =>
      this.marketplaceService.getPackage$(this.pkgId, version),
    ),
  )

  @tuiPure
  getAnimation(duration: number): TuiDurationOptions {
    return { value: '', params: { duration } }
  }

  presentModalImg(content: PolymorpheusContent<TuiDialogContext>) {
    this.dialogs
      .open(content, {
        size: 'auto',
      })
      .subscribe()
  }
}
