import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router, RouterLink } from '@angular/router'
import { TuiDropdownSheet } from '@taiga-ui/addon-mobile'
import {
  TuiDataList,
  TuiDropdown,
  TuiIcon,
  TuiInput,
  TuiNotificationService,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import {
  TuiBlock,
  TuiNotificationMiddleService,
  TuiSwitch,
} from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'
import { SidebarService } from 'src/app/services/sidebar.service'
import { SystemService } from 'src/app/services/system.service'

@Component({
  selector: 'header',
  template: `
    <button
      class="menu-toggle"
      (click.stop)="sidebars.start.set(!sidebars.start())"
    >
      <tui-icon icon="@tui.menu" />
    </button>
    <tui-textfield iconStart="@tui.search">
      <input tuiInput [(ngModel)]="search" />
    </tui-textfield>
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="sidebars.end" />
      Help
    </label>
    <button class="start9-menu" tuiDropdownSheet tuiDropdown tuiDropdownAuto>
      <img alt="Start9" src="assets/favicon.svg" />
      <tui-data-list *tuiDropdown="let close" size="m" (click)="close()">
        <tui-opt-group>
          <button tuiOption iconStart="@tui.info" (click)="showAbout()">
            About
          </button>
        </tui-opt-group>
        <tui-opt-group>
          <a
            tuiOption
            iconStart="@tui.book-open"
            href="https://docs.start9.com"
            target="_blank"
            rel="noopener"
          >
            Documentation
          </a>
          <a
            tuiOption
            iconStart="@tui.life-buoy"
            href="https://start9.com/contact"
            target="_blank"
            rel="noopener"
          >
            Contact Support
          </a>
        </tui-opt-group>
        <tui-opt-group>
          <a tuiOption iconStart="@tui.settings" routerLink="/settings">
            System Settings
          </a>
        </tui-opt-group>
        <tui-opt-group>
          <button tuiOption iconStart="@tui.log-out" (click)="logout()">
            Logout
          </button>
          <button tuiOption iconStart="@tui.refresh-cw" (click)="restart()">
            Restart
          </button>
        </tui-opt-group>
      </tui-data-list>
    </button>
  `,
  styles: `
    :host {
      grid-column: span 3;
      display: flex;
      align-items: center;
      gap: 0.75rem;
      padding: 0 0.75rem;
      background: var(--tui-background-neutral-2);
      border-bottom: 1px solid var(--tui-border-normal);
    }

    .menu-toggle {
      display: none;
      width: 2rem;
      height: 2rem;
      background: none;
      border: none;
      padding: 0;
      cursor: pointer;
      color: var(--tui-text-primary);
    }

    tui-textfield {
      margin-inline-end: auto;
      width: min(15rem, calc(100vw - 13rem));
    }

    tui-textfield,
    [tuiBlock] {
      border-radius: 2rem;
    }

    .start9-menu {
      width: 2rem;
      height: 2rem;
      background: none;
      border: none;
      padding: 0;
      cursor: pointer;
    }

    :host-context(tui-root._mobile) {
      grid-column: span 1;

      .menu-toggle {
        display: block;
      }
    }

    :host-context(body:not([tuiTheme])) {
      background: var(--tui-background-base);

      .start9-menu {
        filter: invert(1);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    FormsModule,
    RouterLink,
    TuiDataList,
    TuiDropdown,
    TuiIcon,
    TuiInput,
    TuiTextfield,
    TuiBlock,
    TuiSwitch,
    TuiDropdownSheet,
  ],
  providers: [
    tuiTextfieldOptionsProvider({
      size: signal('s'),
      appearance: signal('secondary-grayscale'),
    }),
  ],
})
export class Header {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)
  private readonly router = inject(Router)
  private readonly alerts = inject(TuiNotificationService)
  private readonly loading = inject(TuiNotificationMiddleService)

  protected readonly sidebars = inject(SidebarService)
  protected readonly system = inject(SystemService)
  protected readonly search = signal('')

  protected showAbout(): void {
    const info = this.system.info()
    if (info) {
      this.alerts
        .open(`Version: ${info.version}`, { label: 'About' })
        .subscribe()
    }
  }

  protected async logout(): Promise<void> {
    const loading = this.loading.open('').subscribe()
    try {
      await this.api.logout()
      this.auth.authenticated.set(false)
      this.router.navigate(['.'])
    } catch (e: any) {
      console.error(e)
      this.alerts.open(e, { appearance: 'negative' }).subscribe()
    } finally {
      loading.unsubscribe()
    }
  }

  protected async restart(): Promise<void> {
    const loading = this.loading.open('Restarting...').subscribe()
    try {
      await this.api.systemRestart()
      this.alerts
        .open('Router is restarting', { appearance: 'info' })
        .subscribe()
    } catch (e: any) {
      console.error(e)
      this.alerts.open(e, { appearance: 'negative' }).subscribe()
    } finally {
      loading.unsubscribe()
    }
  }
}
