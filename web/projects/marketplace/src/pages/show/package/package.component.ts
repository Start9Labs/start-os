import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
  Input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { BehaviorSubject } from 'rxjs'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiDurationOptions,
  tuiFadeIn,
} from '@taiga-ui/core'
import { tuiPure } from '@taiga-ui/cdk'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'
import { isPlatform } from '@ionic/angular'
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

  constructor(
    private readonly activatedRoute: ActivatedRoute,
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly pkgId = getPkgId(this.activatedRoute)
  readonly version$ = new BehaviorSubject('*')
  index = 0
  speed = 1000
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')

  @tuiPure
  getAnimation(duration: number): TuiDurationOptions {
    return { value: '', params: { duration } }
  }

  presentModalImg(content: PolymorpheusContent<TuiDialogContext>) {
    this.dialogs
      .open(content, {
        size: 'l',
      })
      .subscribe()
  }
}
