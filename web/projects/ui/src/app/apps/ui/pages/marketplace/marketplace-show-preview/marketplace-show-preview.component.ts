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
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'marketplace-show-preview',
  templateUrl: './marketplace-show-preview.component.html',
  styleUrls: ['./marketplace-show-preview.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiFadeIn],
})
export class MarketplaceShowPreviewComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  constructor(
    @Inject(TuiDialogService) private readonly dialogs: TuiDialogService,
  ) {}

  readonly sidebarService = inject(SidebarService)
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
