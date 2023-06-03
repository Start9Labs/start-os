import {
  ChangeDetectionStrategy,
  Component,
  Input,
  ViewChild,
} from '@angular/core'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import {
  AddressInfo,
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ErrorToastService } from '@start9labs/shared'
import { AlertController, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  AppConfigPage,
  PackageConfigData,
} from '../../modals/app-config/app-config.page'
import { DependencyInfo } from '../../pipes/to-dependencies.pipe'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ConnectionService } from 'src/app/services/connection.service'
import { PatchDB } from 'patch-db-client'
import { LaunchMenuComponent } from '../../../launch-menu/launch-menu.component'

@Component({
  selector: 'app-show-status',
  templateUrl: './app-show-status.component.html',
  styleUrls: ['./app-show-status.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowStatusComponent {
  @ViewChild('launchMenu') launchMenu!: LaunchMenuComponent

  @Input()
  pkg!: PackageDataEntry

  @Input()
  status!: PackageStatus

  @Input()
  dependencies: DependencyInfo[] = []

  readonly connected$ = this.connectionService.connected$

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly embassyApi: ApiService,
    private readonly formDialog: FormDialogService,
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  private get id(): string {
    return this.pkg.manifest.id
  }

  get addressInfo(): Record<string, AddressInfo> {
    return this.pkg.installed!['address-info']
  }

  get isConfigured(): boolean {
    return this.pkg.installed!.status.configured
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

  get rendering(): StatusRendering {
    return PrimaryRendering[this.status.primary]
  }

  openPopover(e: Event): void {
    this.launchMenu.event = e
    this.launchMenu.isOpen = true
  }

  presentModalConfig(): void {
    this.formDialog.open<PackageConfigData>(AppConfigPage, {
      label: `${this.pkg.manifest.title} configuration`,
      data: { pkgId: this.id },
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
    const { title, alerts, id } = this.pkg.manifest

    let message = alerts.stop || ''
    if (await hasCurrentDeps(this.patch, id)) {
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
    const { id, title } = this.pkg.manifest

    if (await hasCurrentDeps(this.patch, id)) {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
        message: `Services that depend on ${title} may temporarily experiences issues`,
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
