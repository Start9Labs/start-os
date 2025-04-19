import { inject, Injectable } from '@angular/core'
import { MarketplacePkgBase } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceAlertsService {
  private readonly dialog = inject(DialogService)
  private readonly marketplace$ = inject<PatchDB<DataModel>>(PatchDB).watch$(
    'ui',
    'marketplace',
  )
  private readonly i18n = inject(i18nPipe)

  async alertMarketplace(url: string, originalUrl: string): Promise<boolean> {
    const marketplaces = await firstValueFrom(this.marketplace$)
    const name = marketplaces.knownHosts[url]?.name || url
    const source = marketplaces.knownHosts[originalUrl]?.name || originalUrl
    const message = source
      ? `${this.i18n.transform('installed from')} ${source}`
      : this.i18n.transform('sideloaded')

    return new Promise(async resolve => {
      this.dialog
        .openConfirm<boolean>({
          label: 'Warning',
          size: 's',
          data: {
            content:
              `${this.i18n.transform('This service was originally')} ${message}, ${this.i18n.transform('but you are currently connected to')} ${name}. ${this.i18n.transform('To install from')} ${name} ${this.i18n.transform('anyway, click "Continue".')}` as i18nKey,
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
