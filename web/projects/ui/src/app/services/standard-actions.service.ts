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
import { RR } from './api/api.types'

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
      await this.router.navigate(['services', id])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async uninstall(
    { id, title, alerts }: T.Manifest,
    { force, soft }: { force: boolean; soft: boolean } = {
      force: false,
      soft: false,
    },
  ): Promise<void> {
    let content = soft
      ? ''
      : alerts.uninstall ||
        `${this.i18n.transform('Uninstalling')} ${title} ${this.i18n.transform('will permanently delete its data.')}`

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      content = `${content}${content ? ' ' : ''}${this.i18n.transform('Services that depend on')} ${title} ${this.i18n.transform('will no longer work properly and may crash.')}`
    }

    if (!content) {
      return this.doUninstall({ id, force, soft })
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
      .subscribe(() => this.doUninstall({ id, force, soft }))
  }

  private async doUninstall(options: RR.UninstallPackageReq) {
    const loader = this.loader.open('Beginning uninstall').subscribe()

    try {
      await this.api.uninstallPackage(options)
      await this.api.setDbValue<boolean>(['ackInstructions', options.id], false)
      await this.router.navigate([''])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
