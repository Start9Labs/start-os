import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AlertController, NavController } from '@ionic/angular'
import { ErrorService, getPkgId, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ActionService } from 'src/app/services/action.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { filter, map } from 'rxjs'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsPage {
  readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.patch.watch$('packageData', this.pkgId).pipe(
    filter(pkg => pkg.stateInfo.state === 'installed'),
    map(pkg => ({
      mainStatus: pkg.status.main,
      manifest: getManifest(pkg),
      actions: Object.keys(pkg.actions)
        .filter(id => id !== 'config')
        .map(id => ({
          id,
          ...pkg.actions[id],
        })),
    })),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly api: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly navCtrl: NavController,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionService: ActionService,
  ) {}

  async handleAction(
    mainStatus: T.MainStatus['main'],
    manifest: T.Manifest,
    action: T.ActionMetadata & { id: string },
  ) {
    this.actionService.present(
      { id: manifest.id, title: manifest.title, mainStatus },
      { id: action.id, metadata: action },
    )
  }

  async tryUninstall(manifest: T.Manifest): Promise<void> {
    let message =
      manifest.alerts.uninstall ||
      `Uninstalling ${manifest.title} will permanently delete its data`

    if (hasCurrentDeps(this.pkgId, await getAllPackages(this.patch))) {
      message = `${message}. Services that depend on ${manifest.title} will no longer work properly and may crash`
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
      await this.api.uninstallPackage({ id: this.pkgId })
      this.api
        .setDbValue<boolean>(['ackInstructions', this.pkgId], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.navCtrl.navigateRoot('/services')
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

@Component({
  selector: 'app-actions-item',
  templateUrl: './app-actions-item.component.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsItemComponent {
  @Input() action!: {
    name: string
    description: string
    visibility: T.ActionVisibility
  }

  @Input() icon!: string

  get disabledText() {
    return (
      typeof this.action.visibility === 'object' &&
      this.action.visibility.disabled.reason
    )
  }
}
