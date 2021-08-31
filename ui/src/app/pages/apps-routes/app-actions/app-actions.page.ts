import { Component, Input, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AlertController, IonContent, LoadingController, ModalController, NavController } from '@ionic/angular'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Action, Manifest, PackageDataEntry, PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { Subscription } from 'rxjs'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { AppRestoreComponent } from 'src/app/modals/app-restore/app-restore.component'
import { isEmptyObject } from 'src/app/util/misc.util'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
})
export class AppActionsPage {
  subs: Subscription[] = []
  @ViewChild(IonContent) content: IonContent

  pkgId: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  ngOnDestroy () {
    this.subs.forEach(sub => sub.unsubscribe())
  }

  async handleAction (pkg: PackageDataEntry, action: { key: string, value: Action }) {
    if ((action.value['allowed-statuses'] as PackageMainStatus[]).includes(pkg.installed.status.main.status)) {
      if (!isEmptyObject(action.value['input-spec'])) {
        const modal = await this.modalCtrl.create({
          component: GenericFormPage,
          componentProps: {
            title: action.value.name,
            spec: action.value['input-spec'],
            buttons: [
              {
                text: 'Execute',
                handler: (value: any) => {
                  return this.executeAction(pkg.manifest.id, action.key, value)
                },
              },
            ],
          },
        })
        await modal.present()
      } else {
        const alert = await this.alertCtrl.create({
          header: 'Confirm',
          message: `Are you sure you want to execute action "${action.value.name}"? ${action.value.warning || ''}`,
          buttons: [
            {
              text: 'Cancel',
              role: 'cancel',
            },
            {
              text: 'Execute',
              handler: () => {
                this.executeAction(pkg.manifest.id, action.key)
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
      let error = null
      if (statuses.length) {
        if (statuses.length > 1) { // oxford comma
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
        message: error || `Action "${action.value.name}" can only be executed when service is ${statusesStr}`,
        buttons: ['OK'],
        cssClass: 'alert-error-message enter-click',
      })
      await alert.present()
    }
  }

  async restore (): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: {
        pkgId: this.pkgId,
      },
      component: AppRestoreComponent,
    })

    modal.onWillDismiss().then(res => {
      if (res.role === 'success') this.navCtrl.back()
    })

    await modal.present()
  }

  async uninstall (manifest: Manifest) {
    const { id, title, version, alerts } = manifest
    const data = await wizardModal(
      this.modalCtrl,
      this.wizardBaker.uninstall({
        id,
        title,
        version,
        uninstallAlert: alerts.uninstall,
      }),
    )

    if (data.cancelled) return
    return this.navCtrl.navigateRoot('/services')
  }

  private async executeAction (pkgId: string, actionId: string, input?: object): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Executing action...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const res = await this.embassyApi.executePackageAction({
        id: pkgId,
        'action-id': actionId,
        input,
      })

      const successAlert = await this.alertCtrl.create({
        header: 'Execution Complete',
        message: res.message.split('\n').join('</br ></br />'),
        buttons: [
          {
            text: 'Ok',
            role: 'cancel',
            cssClass: 'enter-click',
          },
        ],
      })

      setTimeout(() => successAlert.present(), 400)

    } catch (e) {
      this.errToast.present(e)
      return false
    } finally {
      loader.dismiss()
    }
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
