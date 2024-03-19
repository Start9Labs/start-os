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
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { hasCurrentDeps } from 'src/app/util/has-deps'

@Injectable({
  providedIn: 'root',
})
export class ActionsService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly formDialog = inject(FormDialogService)

  configure({ manifest }: PackageDataEntry): void {
    this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: `${manifest.title} configuration`,
      data: { pkgId: manifest.id },
    })
  }

  async start({ manifest }: PackageDataEntry, unmet: boolean): Promise<void> {
    const deps = `${manifest.title} has unmet dependencies. It will not work as expected.`

    if (
      (!unmet || (await this.alert(deps))) &&
      (!manifest.alerts.start || (await this.alert(manifest.alerts.start)))
    ) {
      this.doStart(manifest.id)
    }
  }

  stop(pkg: PackageDataEntry): void {
    const { title, alerts } = pkg.manifest

    let content = alerts.stop || ''

    if (hasCurrentDeps(pkg)) {
      const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
      content = content ? `${content}.\n\n${depMessage}` : depMessage
    }

    if (content) {
      this.dialogs
        .open(TUI_PROMPT, getOptions(content, 'Stop'))
        .pipe(filter(Boolean))
        .subscribe(() => this.doStop(pkg.manifest.id))
    } else {
      this.doStop(pkg.manifest.id)
    }
  }

  restart(pkg: PackageDataEntry): void {
    if (hasCurrentDeps(pkg)) {
      this.dialogs
        .open(
          TUI_PROMPT,
          getOptions(
            `Services that depend on ${pkg.manifest} may temporarily experiences issues`,
            'Restart',
          ),
        )
        .pipe(filter(Boolean))
        .subscribe(() => this.doRestart(pkg.manifest.id))
    } else {
      this.doRestart(pkg.manifest.id)
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
