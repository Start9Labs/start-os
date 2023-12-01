import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiAlertService, TuiDialogService } from '@taiga-ui/core'
import { TuiIconModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiCheckboxLabeledModule } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ConfigService } from 'src/app/services/config.service'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  template: `
    <ng-container *ngIf="server$ | async as server">
      <button class="g-action" (click)="reset(tor)">
        <tui-icon icon="tuiIconRotateCw" />
        <div tuiTitle>
          <strong>Reset Tor</strong>
          <div tuiSubtitle>
            Resetting the Tor daemon on your server may resolve Tor connectivity
            issues.
          </div>
        </div>
      </button>
      <button class="g-action" (click)="zram(server.zram)">
        <tui-icon [icon]="server.zram ? 'tuiIconZapOff' : 'tuiIconZap'" />
        <div tuiTitle>
          <strong>{{ server.zram ? 'Disable' : 'Enable' }} zram</strong>
          <div tuiSubtitle>
            Zram creates compressed swap in memory, resulting in faster I/O for
            low RAM devices
          </div>
        </div>
      </button>
    </ng-container>

    <ng-template #tor>
      <p *ngIf="isTor">
        You are currently connected over Tor. If you reset the Tor daemon, you
        will lose connectivity until it comes back online.
      </p>
      <p *ngIf="!isTor">Reset Tor?</p>
      <p>
        Optionally wipe state to forcibly acquire new guard nodes. It is
        recommended to try without wiping state first.
      </p>
      <tui-checkbox-labeled size="l" [(ngModel)]="wipe">
        Wipe state
      </tui-checkbox-labeled>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiTitleModule,
    TuiIconModule,
    TuiCheckboxLabeledModule,
  ],
})
export class SettingsExperimentalComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly alerts = inject(TuiAlertService)

  readonly server$ = inject(PatchDB<DataModel>).watch$('server-info')
  readonly isTor = inject(ConfigService).isTor()

  wipe = false

  reset(content: TemplateRef<any>) {
    this.wipe = false
    this.dialogs
      .open(TUI_PROMPT, {
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content,
          yes: 'Reset',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.resetTor(this.wipe))
  }

  zram(enabled: boolean) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        data: {
          content: enabled
            ? 'Are you sure you want to disable zram? It provides significant performance benefits on low RAM devices.'
            : 'Enable zram? It will only make a difference on lower RAM devices.',
          yes: enabled ? 'Disable' : 'Enable',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.toggleZram(enabled))
  }

  private async toggleZram(enabled: boolean) {
    const loader = this.loader
      .open(enabled ? 'Disabling zram...' : 'Enabling zram...')
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
}
