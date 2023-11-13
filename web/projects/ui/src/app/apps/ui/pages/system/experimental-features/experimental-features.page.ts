import {
  ChangeDetectionStrategy,
  Component,
  TemplateRef,
  ViewChild,
} from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter } from 'rxjs'

@Component({
  selector: 'experimental-features',
  templateUrl: './experimental-features.page.html',
  styleUrls: ['./experimental-features.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ExperimentalFeaturesPage {
  readonly server$ = this.patch.watch$('server-info')

  @ViewChild('tor')
  template?: TemplateRef<any>

  wipe = false

  constructor(
    private readonly alerts: TuiAlertService,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly errorService: ErrorService,
  ) {}

  get isTor(): boolean {
    return this.config.isTor()
  }

  async presentAlertResetTor() {
    this.wipe = false
    this.dialogs
      .open(TUI_PROMPT, {
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content: this.template,
          yes: 'Reset',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.resetTor(this.wipe))
  }

  presentAlertZram(enabled: boolean) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: enabled ? 'Confirm' : 'Warning',
        data: {
          content: enabled
            ? 'Are you sure you want to disable zram?'
            : 'zram on StartOS is experimental. It may increase performance of you server, especially if it is a low RAM device.',
          yes: enabled ? 'Disable' : 'Enable',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.toggleZram(enabled))
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open('Resetting Tor...').subscribe()

    try {
      await this.api.resetTor({
        'wipe-state': wipeState,
        reason: 'User triggered',
      })
      this.alerts.open('Tor reset in progress').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async toggleZram(enabled: boolean) {
    const loader = this.loader
      .open(enabled ? 'Disabling zram...' : 'Enabling zram')
      .subscribe()

    try {
      await this.api.toggleZram({ enable: !enabled })
      this.alerts.open(`Zram ${enabled ? 'disabled' : 'enabled'}`).subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
