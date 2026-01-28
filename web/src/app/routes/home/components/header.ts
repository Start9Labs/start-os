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
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiInput,
  TuiNotificationService,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
} from '@taiga-ui/core'
import { TuiBlock, TuiSwitch } from '@taiga-ui/kit'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'
import { SidebarService } from 'src/app/services/sidebar.service'
import { SystemService } from 'src/app/services/system.service'

@Component({
  selector: 'header',
  template: `
    <button
      size="s"
      appearance="icon"
      iconStart="@tui.menu"
      class="nav-menu g-primary"
      tuiIconButton
      (click.stop)="sidebars.start.set(!sidebars.start())"
    >
      Menu
    </button>
    <tui-textfield iconStart="@tui.search">
      <input tuiInput [(ngModel)]="search" />
    </tui-textfield>
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="sidebars.end" />
      Help
    </label>
    <button
      size="s"
      appearance=""
      class="start9-menu"
      tuiIconButton
      tuiDropdown
      tuiDropdownAuto
      tuiDropdownSheet
    >
      <img alt="Start9" src="assets/favicon.svg" [style.margin]="0" />
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

      tui-textfield,
      [tuiBlock] {
        border-radius: 2rem;
      }
    }

    tui-textfield {
      margin-inline-end: auto;
      width: min(12.5rem, calc(100vw - 13rem));
    }

    .nav-menu {
      display: none;
    }

    :host-context(tui-root._mobile) {
      grid-column: span 1;

      .nav-menu {
        display: flex;
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
    TuiInput,
    TuiTextfield,
    TuiBlock,
    TuiSwitch,
    TuiDropdownSheet,
    TuiButton,
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
  private readonly actions = inject(ActionService)
  private readonly alerts = inject(TuiNotificationService)

  protected readonly sidebars = inject(SidebarService)
  protected readonly system = inject(SystemService)
  protected readonly search = signal('')

  protected showAbout(): void {
    if (this.system.info()) {
      this.alerts
        .open(`Version: ${this.system.info()?.version}`, { label: 'About' })
        .subscribe()
    }
  }

  protected async logout(): Promise<void> {
    if (await this.actions.run(() => this.api.logout(), { loading: '' })) {
      this.auth.authenticated.set(false)
      this.router.navigate(['.'])
    }
  }

  protected async restart(): Promise<void> {
    await this.actions.run(() => this.api.systemRestart(), {
      loading: 'Restarting...',
      success: 'Router is restarting',
    })
  }
}
