import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  TuiDataListModule,
  TuiDialogOptions,
  TuiDialogService,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { ABOUT } from './about.component'
import { getAllPackages } from 'src/app/util/get-package-data'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { HeaderConnectionComponent } from './connection.component'

@Component({
  selector: 'header-menu',
  template: `
    <tui-hosted-dropdown [content]="content" [tuiDropdownMaxHeight]="9999">
      <button tuiIconButton appearance="">
        <img style="max-width: 62%" src="assets/img/icon.png" alt="StartOS" />
      </button>
      <ng-template #content>
        <tui-data-list>
          <header-connection class="status">
            <h3 class="title">StartOS</h3>
          </header-connection>
          <button tuiOption class="item" (click)="about()">
            <tui-icon icon="tuiIconInfo" />
            About this server
          </button>
          <tui-opt-group>
            @for (link of links; track $index) {
              <a
                tuiOption
                class="item"
                target="_blank"
                rel="noreferrer"
                [href]="link.href"
              >
                <tui-icon [icon]="link.icon" />
                {{ link.name }}
                <tui-icon class="external" icon="tuiIconArrowUpRight" />
              </a>
            }
          </tui-opt-group>
          <tui-opt-group>
            @for (item of system; track $index) {
              <button tuiOption class="item" (click)="prompt(item.action)">
                <tui-icon [icon]="item.icon" />
                {{ item.action }}
              </button>
            }
          </tui-opt-group>
          <tui-opt-group>
            <button tuiOption class="item" (click)="logout()">
              <tui-icon icon="tuiIconLogOut" />
              Logout
            </button>
          </tui-opt-group>
        </tui-data-list>
      </ng-template>
    </tui-hosted-dropdown>
  `,
  styles: [
    `
      tui-icon {
        font-size: 1rem;
      }

      .item {
        justify-content: flex-start;
        gap: 0.75rem;
      }

      .status {
        display: flex !important;
        font-size: 0;
        padding: 0 0.5rem;
        height: 2rem;
        width: 14rem;
      }

      .title {
        margin: 0 auto 0 0;
        font: var(--tui-font-text-l);
        font-weight: bold;
      }

      .external {
        margin-left: auto;
        padding-left: 0.5rem;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    TuiButtonModule,
    TuiIconModule,
    HeaderConnectionComponent,
  ],
})
export class HeaderMenuComponent {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly auth = inject(AuthService)
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly dialogs = inject(TuiDialogService)

  readonly links = [
    {
      name: 'User Manual',
      icon: 'tuiIconBookOpen',
      href: 'https://docs.start9.com/0.3.5.x/user-manual',
    },
    {
      name: 'Contact Support',
      icon: 'tuiIconHeadphones',
      href: 'https://start9.com/contact',
    },
    {
      name: 'Donate to Start9',
      icon: 'tuiIconDollarSign',
      href: 'https://donate.start9.com',
    },
  ]

  readonly system = [
    {
      icon: 'tuiIconTool',
      action: 'System Rebuild',
    },
    {
      icon: 'tuiIconRefreshCw',
      action: 'Restart',
    },
    {
      icon: 'tuiIconPower',
      action: 'Shutdown',
    },
  ] as const

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }

  async prompt(action: keyof typeof METHODS) {
    const minutes =
      action === 'System Rebuild'
        ? Object.keys(await getAllPackages(this.patch)).length * 2
        : ''

    this.dialogs
      .open(TUI_PROMPT, getOptions(action, minutes))
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open(`Beginning ${action}...`).subscribe()

        try {
          await this.api[METHODS[action]]({})
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}

const METHODS = {
  Restart: 'restartServer',
  Shutdown: 'shutdownServer',
  'System Rebuild': 'systemRebuild',
} as const

function getOptions(
  key: keyof typeof METHODS,
  minutes: unknown,
): Partial<TuiDialogOptions<TuiPromptData>> {
  switch (key) {
    case 'Restart':
      return {
        label: 'Restart',
        size: 's',
        data: {
          content:
            'Are you sure you want to restart your server? It can take several minutes to come back online.',
          yes: 'Restart',
          no: 'Cancel',
        },
      }
    case 'Shutdown':
      return {
        label: 'Warning',
        size: 's',
        data: {
          content:
            'Are you sure you want to power down your server? This can take several minutes, and your server will not come back online automatically. To power on again, You will need to physically unplug your server and plug it back in',
          yes: 'Shutdown',
          no: 'Cancel',
        },
      }
    default:
      return {
        label: 'Warning',
        size: 's',
        data: {
          content: `This action will tear down all service containers and rebuild them from scratch. No data will be deleted. This action is useful if your system gets into a bad state, and it should only be performed if you are experiencing general performance or reliability issues. It may take up to ${minutes} minutes to complete. During this time, you will lose all connectivity to your server.`,
          yes: 'Rebuild',
          no: 'Cancel',
        },
      }
  }
}
