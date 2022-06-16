import { Component, Input, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  AlertController,
  IonContent,
  IonicSafeString,
  LoadingController,
  ModalController,
  NavController,
} from '@ionic/angular'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import {
  Action,
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { Subscription } from 'rxjs'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { isEmptyObject, ErrorToastService, getPkgId } from '@start9labs/shared'
import { ActionSuccessPage } from 'src/app/modals/action-success/action-success.page'
import { hasCurrentDeps } from 'src/app/util/has-deps'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
})
export class AppActionsPage {
  @ViewChild(IonContent) content: IonContent
  readonly pkgId = getPkgId(this.route)
  pkg: PackageDataEntry
  subs: Subscription[]

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    this.subs = [
      this.patch.watch$('package-data', this.pkgId).subscribe(pkg => {
        this.pkg = pkg
      }),
    ]
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy() {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async handleAction(action: { key: string; value: Action }) {
    const status = this.pkg.installed?.status
    if (
      status &&
      (action.value['allowed-statuses'] as PackageMainStatus[]).includes(
        status.main.status,
      )
    ) {
      if (!isEmptyObject(action.value['input-spec'] || {})) {
        const modal = await this.modalCtrl.create({
          component: GenericFormPage,
          componentProps: {
            title: action.value.name,
            spec: action.value['input-spec'],
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
      const statuses = [...action.value['allowed-statuses']]
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
        error = `There is state for which this action may be run. This is a bug. Please file an issue with the service maintainer.`
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

  async tryUninstall(): Promise<void> {
    const { title, alerts } = this.pkg.manifest

    let message =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (hasCurrentDeps(this.pkg)) {
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
        'action-id': actionId,
        input,
      })
      this.modalCtrl.dismiss()
      const successModal = await this.modalCtrl.create({
        component: ActionSuccessPage,
        componentProps: {
          actionRes: res,
        },
      })

      setTimeout(() => successModal.present(), 400)
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
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
})
export class AppActionsItemComponent {
  @Input() action: LocalAction
}
