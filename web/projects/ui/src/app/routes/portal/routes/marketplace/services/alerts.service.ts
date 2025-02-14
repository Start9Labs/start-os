import { TUI_CONFIRM } from '@taiga-ui/kit'
import { inject, Injectable } from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, firstValueFrom } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class MarketplaceAlertsService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly marketplace$ = inject<PatchDB<DataModel>>(PatchDB).watch$(
    'ui',
    'marketplace',
  )

  async alertMarketplace(url: string, originalUrl: string): Promise<boolean> {
    const marketplaces = await firstValueFrom(this.marketplace$)
    const name = marketplaces.knownHosts[url]?.name || url
    const source = marketplaces.knownHosts[originalUrl]?.name || originalUrl
    const message = source ? `installed from ${source}` : 'side loaded'

    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_CONFIRM, {
          label: 'Warning',
          size: 's',
          data: {
            content: `This service was originally ${message}, but you are currently connected to ${name}. To install from ${name} anyway, click "Continue".`,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(defaultIfEmpty(false))
        .subscribe(response => resolve(response))
    })
  }

  async alertBreakages(breakages: string[]): Promise<boolean> {
    let content: string =
      'As a result of this update, the following services will no longer work properly and may crash:<ul>'
    const bullets = breakages.map(title => `<li><b>${title}</b></li>`)
    content = `${content}${bullets.join('')}</ul>`

    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_CONFIRM, {
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

  async alertInstall({ alerts }: MarketplacePkg): Promise<boolean> {
    const content = alerts.install

    return (
      !!content &&
      new Promise(resolve => {
        this.dialogs
          .open<boolean>(TUI_CONFIRM, {
            label: 'Alert',
            size: 's',
            data: {
              content,
              yes: 'Install',
              no: 'Cancel',
            },
          })
          .pipe(defaultIfEmpty(false))
          .subscribe(response => resolve(response))
      })
    )
  }
}
