import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import {
  InterfaceInfo,
  PackageMainStatus,
  PackagePlus,
} from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ServiceConfigModal } from '../modals/config.component'
import { PackageConfigData } from '../types/package-config-data'

@Component({
  selector: 'service-actions',
  template: `
    <button
      *ngIf="isRunning"
      tuiButton
      appearance="secondary-destructive"
      icon="tuiIconSquare"
      (click)="tryStop()"
    >
      Stop
    </button>

    <button
      *ngIf="isRunning"
      tuiButton
      appearance="secondary"
      icon="tuiIconRotateCw"
      (click)="tryRestart()"
    >
      Restart
    </button>

    <button
      *ngIf="isStopped && isConfigured"
      tuiButton
      icon="tuiIconPlay"
      (click)="tryStart()"
    >
      Start
    </button>

    <button
      *ngIf="!isConfigured"
      tuiButton
      appearance="secondary-warning"
      icon="tuiIconTool"
      (click)="presentModalConfig()"
    >
      Configure
    </button>
  `,
  styles: [':host { display: flex; gap: 1rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiButtonModule],
})
export class ServiceActionsComponent {
  @Input({ required: true })
  service!: PackagePlus

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly embassyApi: ApiService,
    private readonly formDialog: FormDialogService,
  ) {}

  private get id(): string {
    return this.service.pkg.manifest.id
  }

  get interfaceInfo(): Record<string, InterfaceInfo> {
    return this.service.pkg.installed!['interfaceInfo']
  }

  get isConfigured(): boolean {
    return this.service.pkg.installed!.status.configured
  }

  get isRunning(): boolean {
    return (
      this.service.pkg.installed?.status.main.status ===
      PackageMainStatus.Running
    )
  }

  get isStopped(): boolean {
    return (
      this.service.pkg.installed?.status.main.status ===
      PackageMainStatus.Stopped
    )
  }

  presentModalConfig(): void {
    this.formDialog.open<PackageConfigData>(ServiceConfigModal, {
      label: `${this.service.pkg.manifest.title} configuration`,
      data: { pkgId: this.id },
    })
  }

  async tryStart(): Promise<void> {
    const pkg = this.service.pkg
    if (Object.values(this.service.dependencies).some(dep => !!dep.errorText)) {
      const depErrMsg = `${pkg.manifest.title} has unmet dependencies. It will not work as expected.`
      const proceed = await this.presentAlertStart(depErrMsg)

      if (!proceed) return
    }

    const alertMsg = pkg.manifest.alerts.start

    if (alertMsg) {
      const proceed = await this.presentAlertStart(alertMsg)

      if (!proceed) return
    }

    this.start()
  }

  async tryStop(): Promise<void> {
    const { title, alerts } = this.service.pkg.manifest

    let content = alerts.stop || ''
    if (hasCurrentDeps(this.service.pkg)) {
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
    if (hasCurrentDeps(this.service.pkg)) {
      this.dialogs
        .open(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `Services that depend on ${this.service.pkg.manifest} may temporarily experiences issues`,
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
