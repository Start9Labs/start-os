import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { UiLauncherService } from 'src/app/services/ui-launcher.service'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import {
  InterfaceDef,
  PackageDataEntry,
  PackageState,
  Status,
} from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from '@start9labs/shared'
import { AlertController, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ModalService } from 'src/app/services/modal.service'
import { DependencyInfo } from '../../pipes/to-dependencies.pipe'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ConnectionService } from 'src/app/services/connection.service'

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

  @Input()
  dependencies: DependencyInfo[] = []

  PR = PrimaryRendering

  readonly connected$ = this.connectionService.connected$

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly embassyApi: ApiService,
    private readonly launcherService: UiLauncherService,
    private readonly modalService: ModalService,
    private readonly connectionService: ConnectionService,
  ) {}

  get interfaces(): Record<string, InterfaceDef> {
    return this.pkg.manifest.interfaces || {}
  }

  get pkgStatus(): Status | null {
    return this.pkg.installed?.status || null
  }

  get isInstalled(): boolean {
    return this.pkg.state === PackageState.Installed
  }

  get isRunning(): boolean {
    return this.status.primary === PrimaryStatus.Running
  }

  get isStopped(): boolean {
    return this.status.primary === PrimaryStatus.Stopped
  }

  launchUi(): void {
    this.launcherService.launch(this.pkg)
  }

  async presentModalConfig(): Promise<void> {
    return this.modalService.presentModalConfig({
      pkgId: this.id,
    })
  }

  async tryStart(): Promise<void> {
    if (this.dependencies.some(d => !!d.errorText)) {
      const depErrMsg = `${this.pkg.manifest.title} has unmet dependencies. It will not work as expected.`
      const proceed = await this.presentAlertStart(depErrMsg)

      if (!proceed) return
    }

    const alertMsg = this.pkg.manifest.alerts.start

    if (alertMsg) {
      const proceed = await this.presentAlertStart(alertMsg)

      if (!proceed) return
    }

    this.start()
  }

  async tryStop(): Promise<void> {
    const { title, alerts } = this.pkg.manifest

    let message = alerts.stop || ''
    if (hasCurrentDeps(this.pkg)) {
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
    if (hasCurrentDeps(this.pkg)) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message: `Services that depend on ${this.pkg.manifest.title} may temporarily experiences issues`,
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

  async presentAlertRestart(): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Are you sure you want to restart this service?',
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
    })

    await alert.present()
  }

  private get id(): string {
    return this.pkg.manifest.id
  }

  private async start(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Starting...`,
    })
    await loader.present()

    try {
      await this.embassyApi.startPackage({ id: this.id })
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
      await this.embassyApi.stopPackage({ id: this.id })
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
      await this.embassyApi.restartPackage({ id: this.id })
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
