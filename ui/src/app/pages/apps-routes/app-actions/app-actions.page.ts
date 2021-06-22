import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { AlertController, ModalController, NavController } from '@ionic/angular'
import { LoaderService } from 'src/app/services/loader.service'
import { HttpErrorResponse } from '@angular/common/http'
import { PatchDbModel } from 'src/app/models/patch-db/patch-db-model'
import { Action, InstalledPackageDataEntry, Manifest, PackageMainStatus } from 'src/app/models/patch-db/data-model'
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
})
export class AppActionsPage {
  pkgId: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loaderService: LoaderService,
    private readonly wizardBaker: WizardBaker,
    private readonly navCtrl: NavController,
    public readonly patch: PatchDbModel,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  async handleAction (pkg: InstalledPackageDataEntry, action: { key: string, value: Action }) {
    if ((action.value['allowed-statuses'] as PackageMainStatus[]).includes(pkg.status.main.status)) {
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
          },
        ],
      })
      await alert.present()
    } else {
      const statuses = [...action.value['allowedStatuses']]
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
        cssClass: 'alert-error-message',
      })
      await alert.present()
    }
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
    return this.navCtrl.navigateRoot('/services/installed')
  }

  private async executeAction (pkgId: string, actionId: string) {
    try {
      const res = await this.loaderService.displayDuringP(
        this.apiService.executePackageAction({ id: pkgId, 'action-id': actionId }),
      )

      const successAlert = await this.alertCtrl.create({
        header: 'Execution Complete',
        message: res.message.split('\n').join('</br ></br />'),
        buttons: ['OK'],
        cssClass: 'alert-success-message',
      })
      return await successAlert.present()
    } catch (e) {
      if (e instanceof HttpErrorResponse) {
        this.presentAlertActionFail(e.status, e.message)
      } else {
        this.presentAlertActionFail(-1, e.message || JSON.stringify(e))
      }
    }
  }

  private async presentAlertActionFail (code: number, message: string): Promise<void> {
    const failureAlert = await this.alertCtrl.create({
      header: 'Execution Failed',
      message: `Error code ${code}. ${message}`,
      buttons: ['OK'],
      cssClass: 'alert-error-message',
    })
    return await failureAlert.present()
  }
}
