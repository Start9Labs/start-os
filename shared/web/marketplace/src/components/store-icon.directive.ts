import { computed, Directive, input } from '@angular/core'
import { knownRegistries, sameUrl } from '@start9labs/shared'

@Directive({
  selector: 'img[storeIcon]',
  host: {
    alt: '',
    '[src]': 'icon()',
  },
})
export class StoreIconDirective {
  readonly storeIcon = input<string>()

  protected readonly icon = computed(() => {
    const { start9Alpha, start9Beta, start9, community } = knownRegistries

    if (sameUrl(this.storeIcon(), start9Alpha)) {
      return 'assets/img/icon_alpha.png'
    } else if (sameUrl(this.storeIcon(), start9Beta)) {
      return 'assets/img/icon_beta.png'
    } else if (sameUrl(this.storeIcon(), start9)) {
      return 'assets/img/icon_transparent.png'
    } else if (sameUrl(this.storeIcon(), community)) {
      return 'assets/img/community-icon.png'
    } else {
      return 'assets/img/storefront-outline.png'
    }
  })
}
