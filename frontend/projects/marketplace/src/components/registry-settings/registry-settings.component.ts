import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
  Input,
  OnDestroy,
} from '@angular/core'
import { FormControl } from '@angular/forms'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject, Subject, takeUntil } from 'rxjs'
import { AbstractMarketplaceService } from '../../services/marketplace.service'
import { StoreIdentity } from '../../types'
import { MarketplaceConfig, sameUrl } from '@start9labs/shared'

@Component({
  selector: 'registry-settings',
  templateUrl: './registry-settings.component.html',
  styleUrls: ['./registry-settings.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class RegistrySettingsComponent implements OnDestroy {
  iconConfig = require('../../../../../config.json').ui.marketplace
  control?: FormControl<StoreIdentity | null>
  loading$ = new BehaviorSubject(false)

  private readonly marketplaceService = inject(AbstractMarketplaceService)
  private readonly hosts$ = this.marketplaceService.getKnownHosts$()
  private url?: string
  private destroy$ = new Subject<void>()

  ngOnInit() {
    this.marketplaceService
      .getSelectedHost$()
      .pipe(takeUntil(this.destroy$))
      .subscribe((val: StoreIdentity) => {
        this.control = new FormControl(val)
        this.url = val.url
      })
  }

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<StoreIdentity>,
  ) {}

  get data(): StoreIdentity[] {
    return this.context.data!
  }

  getName() {
    return (item: StoreIdentity) => item.name!
  }

  submit() {
    this.loading$.next(true)
    setTimeout(() => {
      this.context.completeWith(this.control?.value!)
      this.loading$.next(false)
    }, 800)
  }

  cancel() {
    setTimeout(() => {
      this.context.$implicit.complete()
    }, 100)
  }

  ngOnDestroy(): void {
    this.destroy$.next()
    this.destroy$.complete()
  }

  get icon() {
    const { start9, community } = this.iconConfig

    if (sameUrl(this.url, start9)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.url, community)) {
      return 'assets/img/community-store.png'
    }
    return 'svg/storefront-outline'
  }
}
