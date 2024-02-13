import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplaceConfig, sameUrl } from '@start9labs/shared'

@Component({
  selector: 'store-icon',
  template: `
    <img
      *ngIf="icon; else noIcon"
      [style.border-radius.%]="100"
      [style.max-width]="size || '100%'"
      [src]="icon"
      alt="Service Icon"
    />
    <ng-template #noIcon>
      <tui-icon icon="tuiIconShoppingCart" [style.font-size]="size" />
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class StoreIconComponent {
  @Input()
  url = ''
  @Input()
  size?: string
  @Input({ required: true })
  marketplace!: MarketplaceConfig

  get icon() {
    const { start9, community } = this.marketplace

    if (sameUrl(this.url, start9)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.url, community)) {
      return 'assets/img/community-store.png'
    }
    return null
  }
}
