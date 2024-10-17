import { Injectable } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { hasCurrentDeps } from '../util/has-deps'
import { getAllPackages } from '../util/get-package-data'
import { PatchDB } from 'patch-db-client'
import { DataModel } from './patch-db/data-model'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from './api/embassy-api.service'
import { ErrorService, LoadingService } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class StandardActionsService {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly navCtrl: NavController,
  ) {}

  async rebuild(id: string) {
    const loader = this.loader.open(`Rebuilding Container...`).subscribe()

    try {
      await this.api.rebuildPackage({ id })
      this.navCtrl.navigateBack('/services/' + id)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async tryUninstall(manifest: T.Manifest): Promise<void> {
    const { id, title, alerts } = manifest

    let message =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
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
            this.uninstall(id)
          },
          cssClass: 'enter-click',
        },
      ],
      cssClass: 'alert-warning-message',
    })

    await alert.present()
  }

  private async uninstall(id: string) {
    const loader = this.loader.open(`Beginning uninstall...`).subscribe()

    try {
      await this.api.uninstallPackage({ id })
      this.api
        .setDbValue<boolean>(['ackInstructions', id], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
