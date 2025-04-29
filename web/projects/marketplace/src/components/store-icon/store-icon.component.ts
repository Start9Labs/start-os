import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { knownRegistries, sameUrl } from '@start9labs/shared'

@Component({
  selector: 'store-icon',
  template: `
    <img
      *ngIf="icon; else noIcon"
      [style.border-radius.%]="100"
      [style.max-width]="size || '100%'"
      [src]="icon"
      alt="Registry Icon"
    />
    <ng-template #noIcon>
      <img
        [style.max-width]="size || '100%'"
        src="assets/img/storefront-outline.png"
        alt="Registry Icon"
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

  get icon() {
    const { start9Alpha, start9Beta, start9, community } = knownRegistries

    if (sameUrl(this.url, start9Alpha)) {
      return 'assets/img/icon_alpha.png'
    } else if (sameUrl(this.url, start9Beta)) {
      return 'assets/img/icon_beta.png'
    } else if (sameUrl(this.url, start9)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.url, community)) {
      return 'assets/img/community-icon.png'
    } else {
      return 'assets/img/storefront-outline.png'
    }
  }
}
