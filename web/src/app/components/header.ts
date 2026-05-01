import { KeyValuePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
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
  TuiTitle,
} from '@taiga-ui/core'
import { TuiBlock, TuiSwitch } from '@taiga-ui/kit'
import { HELP, HELP_OPEN } from 'src/app/help/help'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/api.service'
import { AuthService } from 'src/app/services/auth.service'
import { SystemService } from 'src/app/services/system.service'

@Component({
  selector: 'app-header',
  template: `
    <tui-textfield iconStart="@tui.search" [(open)]="open">
      <input
        tuiInput
        placeholder="Search"
        [(ngModel)]="search"
        (input)="open.set(true)"
      />
      @if (results() | keyvalue; as results) {
        @if (search() && results.length) {
          <tui-data-list *tuiDropdown="let close" size="m" (click)="close()">
            @for (result of results; track result.key) {
              @let name = result.key.split('–');
              <a tuiOption [routerLink]="result.value">
                <span tuiTitle>
                  @if (name[0] !== result.key) {
                    <span tuiSubtitle>{{ name[0] }}</span>
                  }
                  {{ name[1] || result.key }}
                </span>
              </a>
            }
          </tui-data-list>
        }
      }
    </tui-textfield>
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="sidebar" />
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
        <button tuiOption iconStart="@tui.info" (click)="showAbout()">
          About
        </button>
        <hr />
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
        <hr />
        <a tuiOption iconStart="@tui.settings" routerLink="/settings">
          System Settings
        </a>
        <hr />
        <button tuiOption iconStart="@tui.log-out" (click)="logout()">
          Logout
        </button>
        <button tuiOption iconStart="@tui.refresh-cw" (click)="restart()">
          Restart
        </button>
      </tui-data-list>
    </button>
  `,
  styles: `
    :host:host {
      display: contents;

      tui-textfield,
      [tuiBlock] {
        border-radius: 2rem;
      }

      tui-textfield {
        margin-inline-end: auto;
        width: min(13.5rem, calc(100vw - 10rem));
      }
    }

    label {
      margin-inline-end: 0.25rem;
    }

    :host-context(body:not([tuiTheme])) .start9-menu {
      filter: invert(1);
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
    KeyValuePipe,
    TuiTitle,
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
  private readonly help = inject(HELP)

  protected readonly sidebar = inject(HELP_OPEN)
  protected readonly system = inject(SystemService)
  protected readonly search = signal('')
  protected readonly open = signal(false)

  protected readonly results = computed(
    (search = this.search().toLowerCase()) =>
      Object.entries(this.help)
        .filter(([_, value]) => value.toLowerCase().includes(search))
        .map(([key, value]) => [
          key,
          value.match(/<h2>(.*?)<\/h2>/s)?.[1] || '',
        ])
        .reduce<Record<string, string>>(
          (result, [url, title]) =>
            result[title]?.length < url.length
              ? result
              : {
                  ...result,
                  [title]: url,
                },
          {},
        ),
  )

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
    await this.actions.run(
      async () => {
        await this.api.systemRestart()
        // Poll until device goes down — the network error propagates
        // to ActionService, which opens the generic reconnect dialog
        while (true) {
          await this.api.systemInfo()
        }
      },
      {
        loading: 'Restarting...',
        success: 'Router is back online',
        restart: true,
      },
    )
  }
}
