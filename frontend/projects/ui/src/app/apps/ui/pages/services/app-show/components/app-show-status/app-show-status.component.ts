import {
  ChangeDetectionStrategy,
  Component,
  Input,
  ViewChild,
} from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import {
  DataModel,
  InterfaceInfo,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  AppConfigPage,
  PackageConfigData,
} from '../../modals/app-config/app-config.page'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ConnectionService } from 'src/app/services/connection.service'
import { LaunchMenuComponent } from '../../../app-list/app-list-pkg/launch-menu/launch-menu.component'

@Component({
  selector: 'app-show-status',
  templateUrl: './app-show-status.component.html',
  styleUrls: ['./app-show-status.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowStatusComponent {
  @ViewChild('launchMenu') launchMenu!: LaunchMenuComponent

  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  status!: PackageStatus

  PR = PrimaryRendering

  readonly connected$ = this.connectionService.connected$

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly formDialog: FormDialogService,
    private readonly connectionService: ConnectionService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  private get id(): string {
    return this.pkg.manifest.id
  }

  get interfaceInfo(): Record<string, InterfaceInfo> {
    return this.pkg.installed!['interfaceInfo']
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

  get canStop(): boolean {
    return [
      PrimaryStatus.Running,
      PrimaryStatus.Starting,
      PrimaryStatus.Restarting,
    ].includes(this.status.primary as PrimaryStatus)
  }

  get isStopped(): boolean {
    return this.status.primary === PrimaryStatus.Stopped
  }

  get rendering(): StatusRendering {
    return PrimaryRendering[this.status.primary]
  }

  presentModalConfig(): void {
    this.formDialog.open<PackageConfigData>(AppConfigPage, {
      label: `${this.pkg.manifest.title} configuration`,
      data: { pkgId: this.id },
    })
  }

  async tryStart(): Promise<void> {
    if (this.status.dependency === 'warning') {
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

    let content = alerts.stop || ''
    if (hasCurrentDeps(this.pkg)) {
      const depMessage = `Services that depend on ${title} will no longer work properly and may crash`
      content = content ? `${content}.\n\n${depMessage}` : depMessage
    }

    if (content) {
      this.dialogs
        .open(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content,
            yes: 'Stop',
            no: 'Cancel',
          },
        })
        .pipe(filter(Boolean))
        .subscribe(() => this.stop())
    } else {
      this.stop()
    }
  }

  async tryRestart(): Promise<void> {
    if (hasCurrentDeps(this.pkg)) {
      this.dialogs
        .open(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `Services that depend on ${this.pkg.manifest} may temporarily experiences issues`,
            yes: 'Restart',
            no: 'Cancel',
          },
        })
        .pipe(filter(Boolean))
        .subscribe(() => this.restart())
    } else {
      this.restart()
    }
  }

  private async start(): Promise<void> {
    const loader = this.loader.open(`Starting...`).subscribe()

    try {
      await this.embassyApi.startPackage({ id: this.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async stop(): Promise<void> {
    const loader = this.loader.open(`Stopping...`).subscribe()

    try {
      await this.embassyApi.stopPackage({ id: this.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async restart(): Promise<void> {
    const loader = this.loader.open(`Restarting...`).subscribe()

    try {
      await this.embassyApi.restartPackage({ id: this.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
  private async presentAlertStart(content: string): Promise<boolean> {
    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }
}
