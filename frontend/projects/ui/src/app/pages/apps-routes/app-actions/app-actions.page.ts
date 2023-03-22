import {
  ChangeDetectionStrategy,
  Component,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
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
  Action,
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import {
  GenericFormPage,
  GenericFormOptions,
} from 'src/app/modals/generic-form/generic-form.page'
import {
  isEmptyObject,
  ErrorToastService,
  getPkgId,
  WithId,
} from '@start9labs/shared'
import { ActionSuccessPage } from 'src/app/modals/action-success/action-success.page'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { filter } from 'rxjs'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsPage {
  readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.patch
    .watch$('package-data', this.pkgId)
    .pipe(filter(pkg => pkg.state === PackageState.Installed))

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

  async handleAction(action: WithId<Action>) {
    if (action.disabled) {
      const alert = await this.alertCtrl.create({
        header: 'Forbidden',
        message: action.disabled,
        buttons: ['OK'],
        cssClass: 'alert-error-message enter-click',
      })
      await alert.present()
    } else {
      if (action['input-spec'] && !isEmptyObject(action['input-spec'])) {
        const options: GenericFormOptions = {
          title: action.name,
          spec: action['input-spec'],
          buttons: [
            {
              text: 'Execute',
              handler: async (value: any) =>
                this.executeAction(action.id, value),
              isSubmit: true,
            },
          ],
        }
        const modal = await this.modalCtrl.create({
          component: GenericFormPage,
          componentProps: options,
        })
        await modal.present()
      } else {
        const alert = await this.alertCtrl.create({
          header: 'Confirm',
          message: `Are you sure you want to execute action "${action.name}"? ${
            action.warning || ''
          }`,
          buttons: [
            {
              text: 'Cancel',
              role: 'cancel',
            },
            {
              text: 'Execute',
              handler: async () => this.executeAction(action.id),
              cssClass: 'enter-click',
            },
          ],
        })
        await alert.present()
      }
    }
  }

  async tryUninstall(pkg: PackageDataEntry): Promise<void> {
    const { title, alerts, id } = pkg.manifest

    let message =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (await hasCurrentDeps(this.patch, id)) {
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
        'action-id': actionId,
        input,
      })

      const successModal = await this.modalCtrl.create({
        component: ActionSuccessPage,
        componentProps: {
          actionRes: res,
        },
      })

      setTimeout(() => successModal.present(), 500)
      return true
    } catch (e: any) {
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
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsItemComponent {
  @Input() action!: LocalAction
}

@Pipe({
  name: 'groupActions',
})
export class GroupActionsPipe implements PipeTransform {
  transform(
    actions: PackageDataEntry['actions'],
  ): Array<Array<WithId<Action>>> | null {
    if (!actions) return null
    const noGroup = 'noGroup'
    const grouped = Object.entries(actions).reduce<
      Record<string, WithId<Action>[]>
    >((groups, [id, action]) => {
      const actionWithId = { id, ...action }
      const groupKey = action.group || noGroup
      if (!groups[groupKey]) {
        groups[groupKey] = [actionWithId]
      } else {
        groups[groupKey].push(actionWithId)
      }
      return groups
    }, {})

    return Object.values(grouped).map(group =>
      group.sort((a, b) => a.name.localeCompare(b.name)),
    )
  }
}
