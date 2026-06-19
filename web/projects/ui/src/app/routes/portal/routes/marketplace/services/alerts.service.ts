import { inject, Injectable } from '@angular/core'
import { MarketplacePkgBase } from '@start9labs/marketplace'
import {
  DialogService,
  i18nKey,
  i18nPipe,
  knownRegistries,
  sameUrl,
} from '@start9labs/shared'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceAlertsService {
  private readonly dialog = inject(DialogService)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly i18n = inject(i18nPipe)

  // Show the registry's caveat when switching to anything other than the
  // official Start9 registry. Replaces the old persistent notification banner.
  alertRegistryChange(url: string): void {
    const message = registryWarning(url)

    if (message) {
      this.dialog.openAlert(message, { label: 'Warning' }).subscribe()
    }
  }

  async alertMarketplace(
    url: string,
    originalUrl: string | null,
  ): Promise<boolean> {
    const registries = await firstValueFrom(this.marketplaceService.registries$)
    const message = originalUrl
      ? `${this.i18n.transform('installed from')} ${registries.find(r => sameUrl(r.url, originalUrl))?.name || originalUrl}`
      : this.i18n.transform('sideloaded')

    const currentName = registries.find(h => sameUrl(h.url, url))?.name || url

    return new Promise(async resolve => {
      this.dialog
        .openConfirm({
          label: 'Warning',
          size: 's',
          data: {
            content:
              `${this.i18n.transform('This service was originally')} ${message}, ${this.i18n.transform('but you are currently connected to')} ${currentName}. ${this.i18n.transform('To install from')} ${currentName} ${this.i18n.transform('anyway, click "Continue".')}` as i18nKey,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(defaultIfEmpty(false))
        .subscribe(response => resolve(response))
    })
  }

  async alertBreakages(breakages: string[]): Promise<boolean> {
    let content =
      `${this.i18n.transform('As a result of this update, the following services will no longer work properly and may crash')}:<ul>` as i18nKey
    const bullets = breakages.map(title => `<li><b>${title}</b></li>`)
    content = `${content}${bullets.join('')}</ul>` as i18nKey

    return new Promise(async resolve => {
      this.dialog
        .openConfirm({
          label: 'Warning',
          size: 's',
          data: {
            content,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(defaultIfEmpty(false))
        .subscribe(response => resolve(response))
    })
  }
}

// The official Start9 registry needs no warning; every other registry carries
// a caveat the user should acknowledge when connecting to it.
function registryWarning(url: string): i18nKey | null {
  const { start9, community, start9Beta, communityBeta, start9Alpha } =
    knownRegistries

  if (sameUrl(url, start9)) {
    return null
  }

  if (sameUrl(url, community)) {
    return 'Services from this registry are packaged and maintained by members of the Start9 community. Install at your own risk. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.'
  }

  if (sameUrl(url, start9Beta) || sameUrl(url, communityBeta)) {
    return 'Services from this registry are undergoing beta testing and may contain bugs. Install at your own risk.'
  }

  if (sameUrl(url, start9Alpha)) {
    return 'Services from this registry are undergoing alpha testing. They are expected to contain bugs and could damage your system. Install at your own risk.'
  }

  return 'This is a Custom Registry. Start9 cannot verify the integrity or functionality of services from this registry, and they could damage your system. Install at your own risk.'
}
