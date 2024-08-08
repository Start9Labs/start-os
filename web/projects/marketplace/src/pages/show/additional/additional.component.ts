import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { CopyService, Exver, MarkdownComponent } from '@start9labs/shared'
import { MarketplacePkg } from '../../../types'
import { AbstractMarketplaceService } from '../../../services/marketplace.service'

@Component({
  selector: 'marketplace-additional',
  templateUrl: 'additional.component.html',
  styleUrls: ['additional.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AdditionalComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  private readonly marketplaceService = inject(AbstractMarketplaceService)

  constructor(
    readonly copyService: CopyService,
    private readonly dialogs: TuiDialogService,
    private readonly route: ActivatedRoute,
  ) {}

  readonly url = this.route.snapshot.queryParamMap.get('url') || undefined

  presentModalMd(label: string) {
    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label,
        size: 'l',
        data: {
          content: this.marketplaceService.fetchStatic$(
            this.pkg.id,
            label.toLowerCase(),
            this.url,
          ),
        },
      })
      .subscribe()
  }
}
