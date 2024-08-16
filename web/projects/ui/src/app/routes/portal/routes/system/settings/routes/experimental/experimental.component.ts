import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TuiAlertService,
  TuiDialogService,
  TuiIcon,
  TuiLabel,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiCheckbox } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <ng-container *ngIf="server$ | async as server">
      <button class="g-action" (click)="reset(tor)">
        <tui-icon icon="@tui.rotate-cw" />
        <div tuiTitle>
          <strong>Reset Tor</strong>
          <div tuiSubtitle>
            Resetting the Tor daemon on your server may resolve Tor connectivity
            issues.
          </div>
        </div>
      </button>
      <button class="g-action" (click)="zram(server.zram)">
        <tui-icon [icon]="server.zram ? '@tui.zap-off' : '@tui.zap'" />
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
      <label tuiLabel>
        <input type="checkbox" tuiCheckbox [(ngModel)]="wipe" />
        Wipe state
      </label>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiTitle,
    TuiIcon,
    TuiLabel,
    TuiCheckbox,
  ],
})
export class SettingsExperimentalComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly alerts = inject(TuiAlertService)

  readonly server$ = inject<PatchDB<DataModel>>(PatchDB).watch$('server-info')
  readonly isTor = inject(ConfigService).isTor()

  wipe = false

  reset(content: TemplateRef<any>) {
    this.wipe = false
    this.dialogs
      .open(TUI_CONFIRM, {
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
