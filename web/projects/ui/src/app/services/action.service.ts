import { inject, Injectable } from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nKey,
  LoadingService,
} from '@start9labs/shared'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter } from 'rxjs'
import {
  ActionInputModal,
  PackageActionData,
} from 'src/app/routes/portal/routes/services/modals/action-input.component'
import { ActionSuccessPage } from 'src/app/routes/portal/routes/services/modals/action-success/action-success.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'

@Injectable({
  providedIn: 'root',
})
export class ActionService {
  private readonly api = inject(ApiService)
  private readonly dialog = inject(DialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly formDialog = inject(FormDialogService)

  async present(data: PackageActionData) {
    const { pkgInfo, actionInfo } = data

    if (actionInfo.metadata.hasInput) {
      this.formDialog.open<PackageActionData>(ActionInputModal, {
        label: actionInfo.metadata.name as i18nKey,
        data,
      })
    } else {
      if (actionInfo.metadata.warning) {
        this.dialog
          .openConfirm({
            label: 'Warning',
            size: 's',
            data: {
              no: 'Cancel',
              yes: 'Run',
              content: actionInfo.metadata.warning as i18nKey,
            },
          })
          .pipe(filter(Boolean))
          .subscribe(() => this.execute(pkgInfo.id, null, actionInfo.id))
      } else {
        this.execute(pkgInfo.id, null, actionInfo.id)
      }
    }
  }

  async execute(
    packageId: string,
    eventId: string | null,
    actionId: string,
    input?: object,
  ) {
    const loader = this.loader.open('Loading').subscribe()

    try {
      const res = await this.api.runAction({
        packageId,
        eventId,
        actionId,
        input: input ?? null,
      })

      if (!res) return

      if (res.result) {
        this.dialog
          .openComponent(new PolymorpheusComponent(ActionSuccessPage), {
            label: res.title as i18nKey,
            data: res,
          })
          .subscribe()
      } else if (res.message) {
        this.dialog
          .openAlert(res.message as i18nKey, { label: res.title as i18nKey })
          .subscribe()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
