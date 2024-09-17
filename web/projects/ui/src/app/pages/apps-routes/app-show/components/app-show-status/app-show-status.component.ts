import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { PatchDB } from 'patch-db-client'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConnectionService } from 'src/app/services/connection.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { UiLauncherService } from 'src/app/services/ui-launcher.service'
import {
  getAllPackages,
  getManifest,
  isInstalled,
  needsConfig,
} from 'src/app/util/get-package-data'
import { hasCurrentDeps } from 'src/app/util/has-deps'

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
  needsConfig = needsConfig

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly launcherService: UiLauncherService,
    readonly connection$: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionService: ActionService,
  ) {}

  get interfaces(): PackageDataEntry['serviceInterfaces'] {
    return this.pkg.serviceInterfaces
  }

  get hosts(): PackageDataEntry['hosts'] {
    return this.pkg.hosts
  }

  get pkgStatus(): T.MainStatus {
    return this.pkg.status
  }

  get manifest(): T.Manifest {
    return getManifest(this.pkg)
  }

  get isRunning(): boolean {
    return this.status.primary === 'running'
  }

  get canStop(): boolean {
    return ['running', 'starting', 'restarting'].includes(this.status.primary)
  }

  get canStart(): boolean {
    return (
      this.status.primary === 'stopped' &&
      !Object.keys(this.pkg.requestedActions).length
    )
  }

  get sigtermTimeout(): string | null {
    return this.pkgStatus?.main === 'stopping' ? '30s' : null // @dr-bonez TODO
  }

  launchUi(
    interfaces: PackageDataEntry['serviceInterfaces'],
    hosts: PackageDataEntry['hosts'],
  ): void {
    this.launcherService.launch(interfaces, hosts)
  }

  async presentModalConfig(): Promise<void> {
    return this.actionService.present(
      {
        id: this.manifest.id,
        title: this.manifest.title,
        mainStatus: this.pkg.status.main,
      },
      { id: 'config', metadata: this.pkg.actions['config'] },
    )
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
    const loader = this.loader.open(`Starting...`).subscribe()

    try {
      await this.embassyApi.startPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async stop(): Promise<void> {
    const loader = this.loader.open('Stopping...').subscribe()

    try {
      await this.embassyApi.stopPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async restart(): Promise<void> {
    const loader = this.loader.open(`Restarting...`).subscribe()

    try {
      await this.embassyApi.restartPackage({ id: this.manifest.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
