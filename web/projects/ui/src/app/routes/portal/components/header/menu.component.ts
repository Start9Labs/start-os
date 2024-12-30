import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  TuiButton,
  TuiDataList,
  TuiDialogService,
  TuiDropdown,
  TuiIcon,
} from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { RESOURCES } from 'src/app/utils/resources'
import { STATUS } from 'src/app/services/status.service'
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
        <button tuiOption iconStart="@tui.info" (click)="about()">
          About this server
        </button>
        <hr />
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
        <hr />
        <a
          tuiOption
          iconStart="@tui.wrench"
          routerLink="/portal/system/settings"
          (click)="open = false"
        >
          System Settings
        </a>
        <hr />
        <button tuiOption iconStart="@tui.log-out" (click)="logout()">
          Logout
        </button>
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
  private readonly dialogs = inject(TuiDialogService)

  open = false

  readonly links = RESOURCES
  readonly status = inject(STATUS)

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }
}
