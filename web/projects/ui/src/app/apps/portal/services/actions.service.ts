import { inject, Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { defaultIfEmpty, filter, firstValueFrom } from 'rxjs'
import {
  PackageConfigData,
  ServiceConfigModal,
} from 'src/app/apps/portal/modals/config.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages } from 'src/app/util/get-package-data'
import { PatchDB } from 'patch-db-client'
import { Manifest } from '../../../../../../../../core/startos/bindings/Manifest'

@Injectable({
  providedIn: 'root',
})
export class ActionsService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)
  private readonly patch = inject(PatchDB<DataModel>)

  configure(manifest: Manifest): void {
    this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: `${manifest.title} configuration`,
      data: { pkgId: manifest.id },
    })
  }

  async start(manifest: Manifest, unmet: boolean): Promise<void> {
    const deps = `${manifest.title} has unmet dependencies. It will not work as expected.`

    if (
      (!unmet || (await this.alert(deps))) &&
      (!manifest.alerts.start || (await this.alert(manifest.alerts.start)))
    ) {
      this.doStart(manifest.id)
    }
  }

  async stop({ id, title, alerts }: Manifest): Promise<void> {
    let content = alerts.stop || ''

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
      content = content ? `${content}.\n\n${depMessage}` : depMessage
    }

    if (content) {
      this.dialogs
        .open(TUI_PROMPT, getOptions(content, 'Stop'))
        .pipe(filter(Boolean))
        .subscribe(() => this.doStop(id))
    } else {
      this.doStop(id)
    }
  }

  async restart({ id, title }: Manifest): Promise<void> {
    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      this.dialogs
        .open(
          TUI_PROMPT,
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
        .open<boolean>(TUI_PROMPT, getOptions(content))
        .pipe(defaultIfEmpty(false)),
    )
  }
}

function getOptions(
  content: string,
  yes = 'Continue',
): Partial<TuiDialogOptions<TuiPromptData>> {
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
