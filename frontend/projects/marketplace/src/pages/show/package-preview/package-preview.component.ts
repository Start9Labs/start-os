import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
  Input,
} from '@angular/core'
import { BehaviorSubject, map } from 'rxjs'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiDurationOptions,
  tuiFadeIn,
} from '@taiga-ui/core'
import { tuiPure } from '@taiga-ui/cdk'
import { PolymorpheusContent } from '@tinkoff/ng-polymorpheus'
import { isPlatform } from '@ionic/angular'
import { MarketplacePkg } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-package-preview',
  templateUrl: './package-preview.component.html',
  styleUrls: ['./package-preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiFadeIn],
})
export class PackagePreviewComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  constructor(
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  private readonly marketplaceService = inject(AbstractMarketplaceService)
  readonly version$ = new BehaviorSubject('*')
  index = 0
  speed = 1000
  isMobile = isPlatform(window, 'ios') || isPlatform(window, 'android')
  url$ = this.marketplaceService.getSelectedHost$().pipe(map(({ url }) => url))

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
