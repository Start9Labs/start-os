import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiButton,
  TuiDataList,
  TuiDialogOptions,
  TuiDropdown,
  TuiIcon,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { STATUS } from 'src/app/services/status.service'
import { RESOURCES } from 'src/app/utils/resources'
import { ABOUT } from './about.component'

@Component({
  selector: 'header-menu',
  template: `
    <button
      tuiIconButton
      appearance=""
      [tuiDropdown]="content"
      [(tuiDropdownOpen)]="open"
      [tuiDropdownMaxHeight]="9999"
    >
      <img [style.max-width.%]="60" src="assets/img/icon.png" alt="StartOS" />
    </button>
    <ng-template #content>
      @if (status().status !== 'success') {
        <div class="status">
          <tui-icon [icon]="status().icon" />
          {{ status().message }}
        </div>
      }
      <tui-data-list [style.width.rem]="13">
        <tui-opt-group>
          <button tuiOption iconStart="@tui.info" (click)="about()">
            About this server
          </button>
        </tui-opt-group>
        <tui-opt-group label="">
          @for (link of links; track $index) {
            <a
              tuiOption
              target="_blank"
              rel="noreferrer"
              [iconStart]="link.icon"
              [href]="link.href"
            >
              {{ link.name }}
            </a>
          }
        </tui-opt-group>
        <tui-opt-group label="">
          <a
            tuiOption
            iconStart="@tui.settings"
            routerLink="/portal/system"
            (click)="open = false"
          >
            System Settings
          </a>
        </tui-opt-group>
        <tui-opt-group label="">
          <button
            tuiOption
            iconStart="@tui.refresh-cw"
            (click)="promptPower('Restart')"
          >
            Restart
          </button>
          <button
            tuiOption
            iconStart="@tui.power"
            (click)="promptPower('Shutdown')"
          >
            Shutdown
          </button>
          <button tuiOption iconStart="@tui.log-out" (click)="logout()">
            Logout
          </button>
        </tui-opt-group>
      </tui-data-list>
    </ng-template>
  `,
  styles: [
    `
      :host {
        padding-inline-start: 0.5rem;

        &._open::before {
          filter: brightness(1.2);
        }
      }

      .status {
        display: flex;
        gap: 1rem;
        align-items: center;
        padding: 1rem 1rem 0.5rem;
        opacity: 0.5;
      }

      [tuiOption] {
        justify-content: flex-start;
        gap: 0.5rem;
      }

      :host-context(tui-root._mobile) {
        [tuiIconButton] {
          box-shadow: inset -1.25rem 0 0 -1rem var(--status);
        }
      }
    `,
  ],
  host: { '[class._open]': 'open' },
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiDropdown, TuiDataList, TuiButton, TuiIcon, RouterLink],
})
export class HeaderMenuComponent {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)

  open = false

  readonly links = RESOURCES
  readonly status = inject(STATUS)

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }

  async promptPower(action: 'Restart' | 'Shutdown') {
    this.dialogs
      .open(TUI_CONFIRM, getOptions(action))
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open(`Beginning ${action}...`).subscribe()

        try {
          await this.api[
            action === 'Restart' ? 'restartServer' : 'shutdownServer'
          ]({})
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }
}

function getOptions(
  operation: 'Restart' | 'Shutdown',
): Partial<TuiDialogOptions<TuiConfirmData>> {
  return operation === 'Restart'
    ? {
        label: 'Restart',
        size: 's',
        data: {
          content:
            'Are you sure you want to restart your server? It can take several minutes to come back online.',
          yes: 'Restart',
          no: 'Cancel',
        },
      }
    : {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, You will need to physically unplug your server and plug it back in',
          yes: 'Shutdown',
          no: 'Cancel',
        },
      }
}
