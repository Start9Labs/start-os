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
import { wizardModal } from 'src/app/components/install-wizard/install-wizard.component'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { WizardBaker } from 'src/app/components/install-wizard/prebaked-wizards'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ModalService } from 'src/app/services/modal.service'
import { DependencyInfo } from '../../pipes/to-dependencies.pipe'

@Component({
  selector: 'app-show-status',
  templateUrl: './app-show-status.component.html',
  styleUrls: ['./app-show-status.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowStatusComponent {
  @Input()
  pkg: PackageDataEntry

  @Input()
  connectionFailure = false

  @Input()
  status: PackageStatus

  @Input()
  dependencies: DependencyInfo[] = []

  PR = PrimaryRendering

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly errToast: ErrorToastService,
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly wizardBaker: WizardBaker,
    private readonly patch: PatchDbService,
    private readonly launcherService: UiLauncherService,
    private readonly modalService: ModalService,
  ) {}

  get interfaces(): Record<string, InterfaceDef> {
    return this.pkg.manifest.interfaces
  }

  get pkgStatus(): Status | null {
    return this.pkg.installed?.status || null
  }

  get isInstalled(): boolean {
    return this.pkg.state === PackageState.Installed && !this.connectionFailure
  }

  get isRunning(): boolean {
    return this.status.primary === PrimaryStatus.Running
  }

  get isStopped(): boolean {
    return (
      this.status.primary === PrimaryStatus.Stopped &&
      !!this.pkgStatus?.configured
    )
  }

  launchUi(): void {
    this.launcherService.launch(this.pkg)
  }

  async presentModalConfig(): Promise<void> {
    return this.modalService.presentModalConfig({ pkgId: this.pkg.manifest.id })
  }

  async tryStart(): Promise<void> {
    if (this.dependencies.some(d => !!d.errorText)) {
      const depErrMsg = `${this.pkg.manifest.title} has unmet dependencies. It will not work as expected.`
      const proceed = await this.presentAlertStart(depErrMsg)

      if (!proceed) return
    }

    const alertMsg = this.pkg.manifest.alerts.start

    if (!!alertMsg) {
      const proceed = await this.presentAlertStart(alertMsg)

      if (!proceed) return
    }

    this.start()
  }

  async stop(): Promise<void> {
    const { id, title, version } = this.pkg.manifest
    const hasDependents = !!Object.keys(
      this.pkg.installed?.['current-dependents'] || {},
    ).filter(depId => depId !== id).length

    if (!hasDependents) {
      const loader = await this.loadingCtrl.create({
        message: `Stopping...`,
        spinner: 'lines',
      })
      await loader.present()

      try {
        await this.embassyApi.stopPackage({ id })
      } catch (e: any) {
        this.errToast.present(e)
      } finally {
        loader.dismiss()
      }
    } else {
      wizardModal(
        this.modalCtrl,
        this.wizardBaker.stop({
          id,
          title,
          version,
        }),
      )
    }
  }

  private async start(): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: `Starting...`,
      spinner: 'lines',
    })
    await loader.present()

    try {
      await this.embassyApi.startPackage({ id: this.pkg.manifest.id })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async presentAlertStart(message: string): Promise<boolean> {
    return new Promise(async resolve => {
      const alert = await this.alertCtrl.create({
        header: 'Warning',
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
