import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink, RouterLinkActive } from '@angular/router'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiHint,
  TuiIcon,
} from '@taiga-ui/core'
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
      tuiHintDirection="bottom"
      [tuiHint]="open ? '' : ('Start Menu' | i18n)"
      [tuiHintShowDelay]="1000"
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
          {{ status().message | i18n }}
        </div>
      }
      <tui-data-list [style.width.rem]="13">
        <tui-opt-group>
          <button tuiOption iconStart="@tui.info" (click)="about()">
            {{ 'About this server' | i18n }}
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
              {{ link.name | i18n }}
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
            {{ 'System Settings' | i18n }}
          </a>
        </tui-opt-group>
        <tui-opt-group label="">
          <button
            tuiOption
            iconStart="@tui.refresh-cw"
            (click)="promptPower('restart')"
          >
            {{ 'Restart' | i18n }}
          </button>
          <button
            tuiOption
            iconStart="@tui.power"
            (click)="promptPower('shutdown')"
          >
            {{ 'Shutdown' | i18n }}
          </button>
          <button tuiOption iconStart="@tui.log-out" (click)="logout()">
            {{ 'Logout' | i18n }}
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
  imports: [
    TuiDropdown,
    TuiDataList,
    TuiButton,
    TuiIcon,
    RouterLink,
    i18nPipe,
    RouterLinkActive,
    TuiHint,
  ],
})
export class HeaderMenuComponent {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly dialog = inject(DialogService)

  open = false

  readonly links = RESOURCES
  readonly status = inject(STATUS)

  about() {
    this.dialog.openComponent(ABOUT, { label: 'About this server' }).subscribe()
  }

  async promptPower(action: 'restart' | 'shutdown') {
    this.dialog
      .openConfirm(
        action === 'restart'
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
                  'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, You will need to physically unplug your server and plug it back in.',
                yes: 'Shutdown',
                no: 'Cancel',
              },
            },
      )
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open(`Beginning ${action}`).subscribe()

        try {
          await this.api[
            action === 'restart' ? 'restartServer' : 'shutdownServer'
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
