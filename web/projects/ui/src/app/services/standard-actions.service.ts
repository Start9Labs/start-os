import { inject, Injectable } from '@angular/core'
import { Router } from '@angular/router'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { getAllPackages } from '../utils/get-package-data'
import { hasCurrentDeps } from '../utils/has-deps'
import { ApiService } from './api/embassy-api.service'
import { DataModel } from './patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class StandardActionsService {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly dialog = inject(DialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly router = inject(Router)
  private readonly i18n = inject(i18nPipe)

  async rebuild(id: string) {
    const loader = this.loader.open('Rebuilding container').subscribe()

    try {
      await this.api.rebuildPackage({ id })
      await this.router.navigate(['portal', 'services', id])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async uninstall({ id, title, alerts }: T.Manifest): Promise<void> {
    let content =
      alerts.uninstall ||
      `${this.i18n.transform('Uninstalling')} ${title} ${this.i18n.transform('will permanently delete its data.')}`

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      content = `${content}. ${this.i18n.transform('Services that depend on')} ${title} ${this.i18n.transform('will no longer work properly and may crash.')}`
    }

    this.dialog
      .openConfirm({
        label: 'Warning',
        size: 's',
        data: {
          content: content as i18nKey,
          yes: 'Uninstall',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.doUninstall(id))
  }

  private async doUninstall(id: string) {
    const loader = this.loader.open('Beginning uninstall').subscribe()

    try {
      await this.api.uninstallPackage({ id })
      await this.api
        .setDbValue<boolean>(['ackInstructions', id], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      await this.router.navigate(['portal'])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
