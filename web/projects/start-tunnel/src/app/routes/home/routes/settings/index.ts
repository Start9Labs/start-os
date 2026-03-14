import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiCell, TuiTitle } from '@taiga-ui/core'
import { TuiBadge, TuiButtonLoading } from '@taiga-ui/kit'
import { TuiCard } from '@taiga-ui/layout'
import { UpdateService } from 'src/app/services/update.service'

import { CHANGE_PASSWORD } from './change-password'

@Component({
  template: `
    <div tuiCardLarge>
      <div tuiCell>
        <span tuiTitle>
          <strong>
            Version
            @if (update.hasUpdate()) {
              <span tuiBadge appearance="positive" size="s">
                Update Available
              </span>
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
  styles: `
    [tuiCardLarge] {
      background: var(--tui-background-neutral-1);

      &:not([data-appearance]) {
        display: none;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiCard, TuiCell, TuiTitle, TuiButton, TuiButtonLoading, TuiBadge],
})
export default class Settings {
  private readonly dialogs = inject(TuiResponsiveDialogService)
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
