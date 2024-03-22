import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  AlertController,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
  PackageMainStatus,
  Status,
} from 'src/app/services/patch-db/data-model'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { isEmptyObject, ErrorToastService, getPkgId } from '@start9labs/shared'
import { ActionSuccessPage } from 'src/app/modals/action-success/action-success.page'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import { ActionMetadata } from '@start9labs/start-sdk/cjs/sdk/lib/types'

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
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async handleAction(
    status: Status,
    action: { key: string; value: ActionMetadata },
  ) {
    if (
      status &&
      action.value.allowedStatuses.includes(
        status.main.status, // @TODO
      )
    ) {
      if (!isEmptyObject(action.value.input || {})) {
        const modal = await this.modalCtrl.create({
          component: GenericFormPage,
          componentProps: {
            title: action.value.name,
            spec: action.value.input,
            buttons: [
              {
                text: 'Execute',
                handler: (value: any) => {
                  return this.executeAction(action.key, value)
                },
                isSubmit: true,
              },
            ],
          },
        })
        await modal.present()
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
      const statuses = [...action.value.allowedStatuses] // @TODO
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
    const loader = await this.loadingCtrl.create({
      message: `Beginning uninstall...`,
    })
    await loader.present()

    try {
      await this.embassyApi.uninstallPackage({ id: this.pkgId })
      this.embassyApi
        .setDbValue<boolean>(['ack-instructions', this.pkgId], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async executeAction(
    actionId: string,
    input?: object,
  ): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      message: 'Executing action...',
    })
    await loader.present()

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
      this.errToast.present(e)
      return false // don't dismiss original modal/alert
    } finally {
      loader.dismiss()
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
