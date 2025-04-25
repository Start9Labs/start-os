import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { knownMarketplaceUrls, sameUrl } from '@start9labs/shared'

@Component({
  selector: 'store-icon',
  template: `
    <img
      *ngIf="icon; else noIcon"
      [style.border-radius.%]="100"
      [style.max-width]="size || '100%'"
      [src]="icon"
      alt="Marketplace Icon"
    />
    <ng-template #noIcon>
      <img
        [style.max-width]="size || '100%'"
        src="assets/img/storefront-outline.png"
        alt="Marketplace Icon"
      />
    </ng-template>
  `,
  styles: ':host { overflow: hidden; }',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class StoreIconComponent {
  @Input()
  url = ''
  @Input()
  size?: string
  @Input({ required: true })
  marketplace!: typeof knownMarketplaceUrls

  get icon() {
    if (sameUrl(this.url, this.marketplace.alpha)) {
      return 'assets/img/icon_alpha.png'
    } else if (sameUrl(this.url, this.marketplace.beta)) {
      return 'assets/img/icon_beta.png'
    } else if (sameUrl(this.url, this.marketplace.prod)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.url, this.marketplace.community)) {
      return 'assets/img/community-icon.png'
    } else {
      return 'assets/img/storefront-outline.png'
    }
  }
}
