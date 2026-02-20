import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiAppearance, TuiButton, TuiTitle } from '@taiga-ui/core'
import { TuiDialogService } from '@taiga-ui/experimental'
import { TuiBadge, TuiButtonLoading } from '@taiga-ui/kit'
import { TuiCard, TuiCell } from '@taiga-ui/layout'
import { UpdateService } from 'src/app/services/update.service'

import { CHANGE_PASSWORD } from './change-password'

@Component({
  template: `
    <div tuiCardLarge tuiAppearance="neutral">
      <div tuiCell>
        <span tuiTitle>
          <strong>
            Version
            @if (update.hasUpdate()) {
              <tui-badge appearance="positive" size="s">
                Update Available
              </tui-badge>
            }
          </strong>
          <span tuiSubtitle>Current: {{ update.installed() ?? '—' }}</span>
        </span>
        @if (update.hasUpdate()) {
          <button tuiButton size="s" [loading]="applying()" (click)="onApply()">
            Update to {{ update.candidate() }}
          </button>
        } @else {
          <button
            tuiButton
            size="s"
            appearance="secondary"
            [loading]="checking()"
            (click)="onCheckUpdate()"
          >
            Check for updates
          </button>
        }
      </div>
      <div tuiCell>
        <span tuiTitle>
          <strong>Change password</strong>
        </span>
        <button tuiButton size="s" (click)="onChangePassword()">Change</button>
      </div>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiCard,
    TuiCell,
    TuiTitle,
    TuiButton,
    TuiButtonLoading,
    TuiBadge,
    TuiAppearance,
  ],
})
export default class Settings {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)

  protected readonly update = inject(UpdateService)
  protected readonly checking = signal(false)
  protected readonly applying = signal(false)

  protected onChangePassword(): void {
    this.dialogs.open(CHANGE_PASSWORD, { label: 'Change Password' }).subscribe()
  }

  protected async onCheckUpdate() {
    this.checking.set(true)

    try {
      await this.update.checkUpdate()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.checking.set(false)
    }
  }

  protected async onApply() {
    this.applying.set(true)

    try {
      await this.update.applyUpdate()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.applying.set(false)
    }
  }
}
