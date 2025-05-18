import { inject, Injectable } from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
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

const allowedStatuses = {
  'only-running': new Set(['running']),
  'only-stopped': new Set(['stopped']),
  any: new Set([
    'running',
    'stopped',
    'restarting',
    'restoring',
    'stopping',
    'starting',
    'backingUp',
  ]),
}

@Injectable({
  providedIn: 'root',
})
export class ActionService {
  private readonly api = inject(ApiService)
  private readonly dialog = inject(DialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly formDialog = inject(FormDialogService)
  private readonly i18n = inject(i18nPipe)

  async present(data: PackageActionData) {
    const { pkgInfo, actionInfo } = data

    if (
      allowedStatuses[actionInfo.metadata.allowedStatuses].has(
        pkgInfo.mainStatus,
      )
    ) {
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
            .subscribe(() => this.execute(pkgInfo.id, actionInfo.id))
        } else {
          this.execute(pkgInfo.id, actionInfo.id)
        }
      }
    } else {
      const statuses = [...allowedStatuses[actionInfo.metadata.allowedStatuses]]
      const last = statuses.pop()
      let statusesStr = statuses.join(', ')
      if (statuses.length) {
        if (statuses.length > 1) {
          // oxford comma
          statusesStr += ','
        }
        statusesStr += ` or ${last}`
      } else if (last) {
        statusesStr = last
      }

      this.dialog
        .openAlert(
          `${this.i18n.transform('Action can only be executed when service is')} ${statusesStr}` as i18nKey,
          { label: 'Forbidden' },
        )
        .pipe(filter(Boolean))
        .subscribe()
    }
  }

  async execute(packageId: string, actionId: string, input?: object) {
    const loader = this.loader.open('Loading').subscribe()

    try {
      const res = await this.api.runAction({
        packageId,
        actionId,
        input: input || null,
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
