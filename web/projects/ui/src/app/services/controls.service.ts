import { inject, Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogOptions, TuiDialogService } from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import {
  defaultIfEmpty,
  defer,
  filter,
  firstValueFrom,
  of,
  switchMap,
} from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

@Injectable({
  providedIn: 'root',
})
export class ControlsService {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  async start({ title, alerts, id }: T.Manifest, unmet: boolean) {
    const deps = `${title} has unmet dependencies. It will not work as expected.`

    if (
      (unmet && !(await this.alert(deps))) ||
      (alerts.start && !(await this.alert(alerts.start)))
    ) {
      return
    }

    const loader = this.loader.open(`Starting...`).subscribe()

    try {
      await this.api.startPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async stop({ id, title, alerts }: T.Manifest) {
    const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
    let content = alerts.stop || ''

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      content = content ? `${content}.\n\n${depMessage}` : depMessage
    }

    defer(() =>
      content
        ? this.dialogs
            .open(TUI_CONFIRM, getOptions(content, 'Stop'))
            .pipe(filter(Boolean))
        : of(null),
    ).subscribe(async () => {
      const loader = this.loader.open(`Stopping...`).subscribe()

      try {
        await this.api.stopPackage({ id })
      } catch (e: any) {
        this.errorService.handleError(e)
      } finally {
        loader.unsubscribe()
      }
    })
  }

  async restart({ id, title }: T.Manifest) {
    const packages = await getAllPackages(this.patch)
    const options = getOptions(
      `Services that depend on ${title} may temporarily experiences issues`,
      'Restart',
    )

    defer(() =>
      hasCurrentDeps(id, packages)
        ? this.dialogs.open(TUI_CONFIRM, options).pipe(filter(Boolean))
        : of(null),
    ).subscribe(async () => {
      const loader = this.loader.open(`Restarting...`).subscribe()

      try {
        await this.api.restartPackage({ id })
      } catch (e: any) {
        this.errorService.handleError(e)
      } finally {
        loader.unsubscribe()
      }
    })
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
