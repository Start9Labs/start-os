import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { UiLauncherService } from 'src/app/services/ui-launcher.service'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import {
  DataModel,
  PackageDataEntry,
  PackageMainStatus,
  Status,
} from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from '@start9labs/shared'
import { AlertController, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ModalService } from 'src/app/services/modal.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ConnectionService } from 'src/app/services/connection.service'
import {
  isInstalled,
  getManifest,
  getAllPackages,
} from 'src/app/util/get-package-data'
import { Manifest } from '@start9labs/marketplace'
import { PatchDB } from 'patch-db-client'

@Component({
  selector: 'app-show-status',
  templateUrl: './app-show-status.component.html',
  styleUrls: ['./app-show-status.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowStatusComponent {
  @Input()
  pkg!: PackageDataEntry

  @Input()
  status!: PackageStatus

  PR = PrimaryRendering

  isInstalled = isInstalled

  readonly connected$ = this.connectionService.connected$

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly embassyApi: ApiService,
    private readonly launcherService: UiLauncherService,
    private readonly modalService: ModalService,
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get interfaces(): PackageDataEntry['service-interfaces'] {
    return this.pkg['service-interfaces']
  }

  get pkgStatus(): Status {
    return this.pkg.status
  }

  get manifest(): Manifest {
    return getManifest(this.pkg)
  }

  get isRunning(): boolean {
    return this.status.primary === PrimaryStatus.Running
  }

  get canStop(): boolean {
    return [
      PrimaryStatus.Running,
      PrimaryStatus.Starting,
      PrimaryStatus.Restarting,
    ].includes(this.status.primary)
  }

  get isStopped(): boolean {
    return this.status.primary === PrimaryStatus.Stopped
  }

  get sigtermTimeout(): string | null {
    return this.pkgStatus?.main.status === PackageMainStatus.Stopping
      ? this.pkgStatus.main.timeout
      : null
  }

  launchUi(interfaces: PackageDataEntry['service-interfaces']): void {
    this.launcherService.launch(interfaces)
  }

  async presentModalConfig(): Promise<void> {
    return this.modalService.presentModalConfig({
      pkgId: this.manifest.id,
    })
  }

  async tryStart(): Promise<void> {
    if (this.status.dependency === 'warning') {
      const depErrMsg = `${this.manifest.title} has unmet dependencies. It will not work as expected.`
      const proceed = await this.presentAlertStart(depErrMsg)

      if (!proceed) return
    }

    const alertMsg = this.manifest.alerts.start

    if (alertMsg) {
      const proceed = await this.presentAlertStart(alertMsg)

      if (!proceed) return
    }

    this.start()
  }

  async tryStop(): Promise<void> {
    const { title, alerts } = this.manifest

    let message = alerts.stop || ''
    if (hasCurrentDeps(this.manifest.id, await getAllPackages(this.patch))) {
      const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
      message = message ? `${message}.\n\n${depMessage}` : depMessage
    }

    if (message) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
          },
          {
            text: 'Stop',
            handler: () => {
              this.stop()
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    } else {
      this.stop()
    }
  }

  async tryRestart(): Promise<void> {
    if (hasCurrentDeps(this.manifest.id, await getAllPackages(this.patch))) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message: `Services that depend on ${this.manifest.title} may temporarily experiences issues`,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
          },
          {
            text: 'Restart',
            handler: () => {
              this.restart()
            },
            cssClass: 'enter-click',
          },
        ],
        cssClass: 'alert-warning-message',
      })

      await alert.present()
    } else {
      this.restart()
    }
  }

  private async start(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Starting...`,
    })
    await loader.present()

    try {
      await this.embassyApi.startPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async stop(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Stopping...',
    })
    await loader.present()

    try {
      await this.embassyApi.stopPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async restart(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Restarting...`,
    })
    await loader.present()

    try {
      await this.embassyApi.restartPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
  private async presentAlertStart(message: string): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Alert',
        message,
        buttons: [
          {
            text: 'Cancel',
            role: 'cancel',
            handler: () => {
              resolve(false)
            },
          },
          {
            text: 'Continue',
            handler: () => {
              resolve(true)
            },
            cssClass: 'enter-click',
          },
        ],
      })

      await alert.present()
    })
  }
}
