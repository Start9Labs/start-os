import { inject, Injectable } from '@angular/core'
import { MarketplacePkgBase } from '@start9labs/marketplace'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { MarketplaceService } from 'src/app/services/marketplace.service'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceAlertsService {
  private readonly dialog = inject(DialogService)
  private readonly marketplaceService = inject(MarketplaceService)
  private readonly i18n = inject(i18nPipe)

  async alertMarketplace(
    url: string,
    originalUrl: string | null,
  ): Promise<boolean> {
    const registries = await firstValueFrom(
      this.marketplaceService.getRegistries$(),
    )
    const message = originalUrl
      ? `${this.i18n.transform('installed from')} ${registries.find(r => r.url === originalUrl)?.name || originalUrl}`
      : this.i18n.transform('sideloaded')

    const currentName = registries.find(h => h.url === url)?.name || url

    return new Promise(async resolve => {
      this.dialog
        .openConfirm<boolean>({
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
      `${this.i18n.transform('As a result of this update, the following services will no longer work properly and may crash')}:<ul>'` as i18nKey
    const bullets = breakages.map(title => `<li><b>${title}</b></li>`)
    content = `${content}${bullets.join('')}</ul>` as i18nKey

    return new Promise(async resolve => {
      this.dialog
        .openConfirm<boolean>({
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

  async alertInstall({ alerts }: MarketplacePkgBase): Promise<boolean> {
    const content = alerts.install

    return (
      !content ||
      (!!content &&
        new Promise(resolve => {
          this.dialog
            .openConfirm<boolean>({
              label: 'Alert',
              size: 's',
              data: {
                content: content as i18nKey,
                yes: 'Install',
                no: 'Cancel',
              },
            })
            .pipe(defaultIfEmpty(false))
            .subscribe(response => resolve(response))
        }))
    )
  }
}
