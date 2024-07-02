import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import {
  ErrorService,
  getPkgId,
  isEmptyObject,
  LoadingService,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { FormComponent } from 'src/app/components/form.component'
import { ActionSuccessPage } from 'src/app/modals/action-success/action-success.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import { hasCurrentDeps } from 'src/app/util/has-deps'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsPage {
  readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.patch.watch$('packageData', this.pkgId)

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async handleAction(
    status: T.Status,
    action: { key: string; value: T.ActionMetadata },
  ) {
    if (status && action.value.allowedStatuses.includes(status.main.status)) {
      if (!isEmptyObject(action.value.input || {})) {
        this.formDialog.open(FormComponent, {
          label: action.value.name,
          data: {
            spec: action.value.input,
            buttons: [
              {
                text: 'Execute',
                handler: async (value: any) =>
                  this.executeAction(action.key, value),
              },
            ],
          },
        })
      } else {
        const alert = await this.alertCtrl.create({
          header: 'Confirm',
          message: `Are you sure you want to execute action "${
            action.value.name
          }"? ${action.value.warning || ''}`,
          buttons: [
            {
              text: 'Cancel',
              role: 'cancel',
            },
            {
              text: 'Execute',
              handler: () => {
                this.executeAction(action.key)
              },
              cssClass: 'enter-click',
            },
          ],
        })
        await alert.present()
      }
    } else {
      const statuses = [...action.value.allowedStatuses]
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
          `Action "${action.value.name}" can only be executed when service is ${statusesStr}`,
        buttons: ['OK'],
        cssClass: 'alert-error-message enter-click',
      })
      await alert.present()
    }
  }

  async tryUninstall(pkg: PackageDataEntry): Promise<void> {
    const { title, alerts } = getManifest(pkg)

    let message =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (hasCurrentDeps(this.pkgId, await getAllPackages(this.patch))) {
      message = `${message}. Services that depend on ${title} will no longer work properly and may crash`
    }

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      message,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Uninstall',
          handler: () => {
            this.uninstall()
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })

    await alert.present()
  }

  private async uninstall() {
    const loader = this.loader.open(`Beginning uninstall...`).subscribe()

    try {
      await this.embassyApi.uninstallPackage({ id: this.pkgId })
      this.embassyApi
        .setDbValue<boolean>(['ack-instructions', this.pkgId], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async executeAction(
    actionId: string,
    input?: object,
  ): Promise<boolean> {
    const loader = this.loader.open('Executing action...').subscribe()

    try {
      const res = await this.embassyApi.executePackageAction({
        id: this.pkgId,
        actionId,
        input,
      })

      const successModal = await this.modalCtrl.create({
        component: ActionSuccessPage,
        componentProps: {
          actionRes: res,
        },
      })

      setTimeout(() => successModal.present(), 500)
      return true // needed to dismiss original modal/alert
    } catch (e: any) {
      this.errorService.handleError(e)
      return false // don't dismiss original modal/alert
    } finally {
      loader.unsubscribe()
    }
  }

  asIsOrder() {
    return 0
  }
}

interface LocalAction {
  name: string
  description: string
  icon: string
}

@Component({
  selector: 'app-actions-item',
  templateUrl: './app-actions-item.component.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsItemComponent {
  @Input() action!: LocalAction
}
