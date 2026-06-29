import { inject, Injectable } from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  i18nService,
} from '@start9labs/shared'
import { T } from '@start9labs/start-core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { defaultIfEmpty, defer, filter, firstValueFrom, of } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages } from 'src/app/utils/get-package-data'
import { hasCurrentDeps } from 'src/app/utils/has-deps'

@Injectable({
  providedIn: 'root',
})
export class ControlsService {
  private readonly dialog = inject(DialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly i18n = inject(i18nPipe)
  private readonly i18nService = inject(i18nService)

  async start({ title, id }: T.Manifest, unmet: boolean) {
    const deps =
      `${title} ${this.i18n.transform('has unmet dependencies. It will not work as expected.')}` as i18nKey

    if (unmet && !(await this.alert(deps))) {
      return
    }

    const loader = this.loader.open('Starting').subscribe()

    try {
      await this.api.startPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async stop({ id, title }: T.Manifest) {
    let content = ''

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
      content = `${this.i18n.transform('Services that depend on')} ${title} ${this.i18n.transform('will no longer work properly and may crash.')}`
    }

    defer(() =>
      content
        ? this.dialog
            .openConfirm({
              label: 'Warning',
              size: 's',
              data: {
                content: content as i18nKey,
                yes: 'Stop',
                no: 'Cancel',
              },
            })
            .pipe(filter(Boolean))
        : of(null),
    ).subscribe(async () => {
      const loader = this.loader.open('Stopping').subscribe()

      try {
        await this.api.stopPackage({ id })
      } catch (e: any) {
        this.errorService.handleError(e)
      } finally {
        loader.unsubscribe()
      }
    })
  }

  async restart(id: string) {
    const loader = this.loader.open('Restarting').subscribe()

    try {
      await this.api.restartPackage({ id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private alert(content: T.LocaleString): Promise<boolean> {
    return firstValueFrom(
      this.dialog
        .openConfirm({
          label: 'Warning',
          size: 's',
          data: {
            content: this.i18nService.localize(content),
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .pipe(defaultIfEmpty(false)),
    )
  }
}
