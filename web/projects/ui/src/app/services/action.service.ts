import { inject, Injectable } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter } from 'rxjs'
import {
  ActionInputModal,
  PackageActionData,
} from 'src/app/routes/portal/routes/service/modals/action-input.component'
import { ActionSuccessPage } from 'src/app/routes/portal/routes/service/modals/action-success/action-success.page'
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
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly formDialog = inject(FormDialogService)

  async present(data: PackageActionData) {
    const { pkgInfo, actionInfo } = data

    if (
      allowedStatuses[actionInfo.metadata.allowedStatuses].has(
        pkgInfo.mainStatus,
      )
    ) {
      if (actionInfo.metadata.hasInput) {
        this.formDialog.open<PackageActionData>(ActionInputModal, {
          label: actionInfo.metadata.name,
          data,
        })
      } else {
        if (actionInfo.metadata.warning) {
          this.dialogs
            .open(TUI_CONFIRM, {
              label: 'Warning',
              size: 's',
              data: {
                no: 'Cancel',
                yes: 'Run',
                content: actionInfo.metadata.warning,
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
      let error = ''
      if (statuses.length) {
        if (statuses.length > 1) {
          // oxford comma
          statusesStr += ','
        }
        statusesStr += ` or ${last}`
      } else if (last) {
        statusesStr = `${last}`
      } else {
        error = `There is no status for which this action may be run. This is a bug. Please file an issue with the service maintainer.`
      }

      this.dialogs
        .open(
          error ||
            `Action "${actionInfo.metadata.name}" can only be executed when service is ${statusesStr}`,
          {
            label: 'Forbidden',
            size: 's',
          },
        )
        .pipe(filter(Boolean))
        .subscribe()
    }
  }

  async execute(packageId: string, actionId: string, input?: object) {
    const loader = this.loader.open('Loading...').subscribe()

    try {
      const res = await this.api.runAction({
        packageId,
        actionId,
        input: input || null,
      })

      if (!res) return

      if (res.result) {
        this.dialogs
          .open(new PolymorpheusComponent(ActionSuccessPage), {
            label: res.title,
            data: res,
          })
          .subscribe()
      } else if (res.message) {
        this.dialogs.open(res.message, { label: res.title }).subscribe()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
