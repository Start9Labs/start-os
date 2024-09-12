import { inject, Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TuiConfirmData, TUI_CONFIRM } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, filter, firstValueFrom } from 'rxjs'
import {
  ConfigModal,
  PackageConfigData,
} from 'src/app/routes/portal/modals/config.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

@Injectable({
  providedIn: 'root',
})
export class ActionsService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  configure(manifest: T.Manifest): void {
    this.formDialog.open<PackageConfigData>(ConfigModal, {
      label: `${manifest.title} configuration`,
      data: { pkgId: manifest.id },
    })
  }

  async start(manifest: T.Manifest, unmet: boolean): Promise<void> {
    const deps = `${manifest.title} has unmet dependencies. It will not work as expected.`

    if (
      (!unmet || (await this.alert(deps))) &&
      (!manifest.alerts.start || (await this.alert(manifest.alerts.start)))
    ) {
      this.doStart(manifest.id)
    }
  }

  async stop({ id, title, alerts }: T.Manifest): Promise<void> {
    let content = alerts.stop || ''

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
      content = content ? `${content}.\n\n${depMessage}` : depMessage
    }

    if (content) {
      this.dialogs
        .open(TUI_CONFIRM, getOptions(content, 'Stop'))
        .pipe(filter(Boolean))
        .subscribe(() => this.doStop(id))
    } else {
      this.doStop(id)
    }
  }

  async restart({ id, title }: T.Manifest): Promise<void> {
    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      this.dialogs
        .open(
          TUI_CONFIRM,
          getOptions(
            `Services that depend on ${title} may temporarily experiences issues`,
            'Restart',
          ),
        )
        .pipe(filter(Boolean))
        .subscribe(() => this.doRestart(id))
    } else {
      this.doRestart(id)
    }
  }

  private async doStart(id: string): Promise<void> {
    const loader = this.loader.open(`Starting...`).subscribe()

    try {
      await this.api.startPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async doStop(id: string): Promise<void> {
    const loader = this.loader.open(`Stopping...`).subscribe()

    try {
      await this.api.stopPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async doRestart(id: string): Promise<void> {
    const loader = this.loader.open(`Restarting...`).subscribe()

    try {
      await this.api.restartPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private alert(content: string): Promise<boolean> {
    return firstValueFrom(
      this.dialogs
        .open<boolean>(TUI_CONFIRM, getOptions(content))
        .pipe(defaultIfEmpty(false)),
    )
  }
}

function getOptions(
  content: string,
  yes = 'Continue',
): Partial<TuiDialogOptions<TuiConfirmData>> {
  return {
    label: 'Warning',
    size: 's',
    data: {
      content,
      yes,
      no: 'Cancel',
    },
  }
}