import { Injectable } from '@angular/core'
import { AlertController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { ActionSuccessPage } from 'src/app/modals/action-success/action-success.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  ActionInputModal,
  PackageActionData,
} from '../modals/action-input.component'

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
  constructor(
    private readonly api: ApiService,
    private readonly dialogs: TuiDialogService,
    private readonly alertCtrl: AlertController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly formDialog: FormDialogService,
  ) {}

  async present(
    pkgInfo: {
      id: string
      title: string
      mainStatus: T.MainStatus['main']
    },
    actionInfo: {
      id: string
      metadata: T.ActionMetadata
    },
    dependentInfo?: {
      title: string
      request: T.ActionRequest
    },
  ) {
    if (
      allowedStatuses[actionInfo.metadata.allowedStatuses].has(
        pkgInfo.mainStatus,
      )
    ) {
      if (actionInfo.metadata.hasInput) {
        this.formDialog.open<PackageActionData>(ActionInputModal, {
          label: actionInfo.metadata.name,
          data: {
            pkgInfo,
            actionInfo: {
              id: actionInfo.id,
              warning: actionInfo.metadata.warning,
            },
            dependentInfo,
          },
        })
      } else {
        const alert = await this.alertCtrl.create({
          header: 'Confirm',
          message: `Are you sure you want to execute action "${
            actionInfo.metadata.name
          }"? ${actionInfo.metadata.warning || ''}`,
          buttons: [
            {
              text: 'Cancel',
              role: 'cancel',
            },
            {
              text: 'Execute',
              handler: () => {
                this.execute(pkgInfo.id, actionInfo.id)
              },
              cssClass: 'enter-click',
            },
          ],
        })
        await alert.present()
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
      const alert = await this.alertCtrl.create({
        header: 'Forbidden',
        message:
          error ||
          `Action "${actionInfo.metadata.name}" can only be executed when service is ${statusesStr}`,
        buttons: ['OK'],
        cssClass: 'alert-error-message enter-click',
      })
      await alert.present()
    }
  }

  async execute(
    packageId: string,
    actionId: string,
    input?: object,
  ): Promise<boolean> {
    const loader = this.loader.open('Loading...').subscribe()

    try {
      const res = await this.api.runAction({
        packageId,
        actionId,
        input: input || null,
      })

      if (res) {
        this.dialogs
          .open(new PolymorpheusComponent(ActionSuccessPage), {
            label: res.name,
            data: res,
          })
          .subscribe()
      }
      return true // needed to dismiss original modal/alert
    } catch (e: any) {
      this.errorService.handleError(e)
      return false // don't dismiss original modal/alert
    } finally {
      loader.unsubscribe()
    }
  }
}
