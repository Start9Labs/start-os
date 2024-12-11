import { inject, Injectable } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
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
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly router = inject(Router)

  async rebuild(id: string) {
    const loader = this.loader.open(`Rebuilding Container...`).subscribe()

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
      `Uninstalling ${title} will permanently delete its data`

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      content = `${content}. Services that depend on ${title} will no longer work properly and may crash`
    }

    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Warning',
        size: 's',
        data: {
          content,
          yes: 'Uninstall',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.doUninstall(id))
  }

  private async doUninstall(id: string) {
    const loader = this.loader.open(`Beginning uninstall...`).subscribe()

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
