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
import { DependencyInfo } from '../../pipes/to-dependencies.pipe'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ConnectionService } from 'src/app/services/connection.service'
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

    let content = alerts.stop || ''
    if (await hasCurrentDeps(this.patch, id)) {
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
    const { id, title } = this.pkg.manifest

    if (await hasCurrentDeps(this.patch, id)) {
      this.dialogs
        .open(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `Services that depend on ${title} may temporarily experiences issues`,
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
