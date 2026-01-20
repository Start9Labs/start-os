import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { Router, RouterLink } from '@angular/router'
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
    <button
      class="start9-menu"
      [tuiDropdown]="menuDropdown"
      [(tuiDropdownOpen)]="menuOpen"
    >
      <img alt="Start9" src="assets/favicon.svg" />
    </button>
    <ng-template #menuDropdown>
      <tui-data-list size="s">
        <button
          tuiOption
          type="button"
          (click)="menuOpen.set(false); showAbout()"
        >
          <tui-icon icon="@tui.info" />
          About
        </button>
        <hr />
        <a
          tuiOption
          href="https://docs.start9.com"
          target="_blank"
          rel="noopener"
        >
          <tui-icon icon="@tui.book-open" />
          Documentation
        </a>
        <a
          tuiOption
          href="https://start9.com/contact"
          target="_blank"
          rel="noopener"
        >
          <tui-icon icon="@tui.life-buoy" />
          Contact Support
        </a>
        <hr />
        <a tuiOption routerLink="/settings" (click)="menuOpen.set(false)">
          <tui-icon icon="@tui.settings" />
          System Settings
        </a>
        <hr />
        <button tuiOption type="button" (click)="logout()">
          <tui-icon icon="@tui.log-out" />
          Logout
        </button>
        <button tuiOption type="button" (click)="restart()">
          <tui-icon icon="@tui.refresh-cw" />
          Restart
        </button>
      </tui-data-list>
    </ng-template>
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

    [tuiInput],
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

    tui-data-list {
      tui-icon {
        font-size: 1rem;
        margin-inline-end: 0.5rem;
      }

      hr {
        height: 1px;
        border: none;
        background: var(--tui-border-normal);
        margin: 0.25rem 0;
      }
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
  protected readonly menuOpen = signal(false)

  protected showAbout(): void {
    const info = this.system.info()
    if (info) {
      this.alerts
        .open(`Version: ${info.version}`, { label: 'About' })
        .subscribe()
    }
  }

  protected async logout(): Promise<void> {
    this.menuOpen.set(false)
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
    this.menuOpen.set(false)
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
