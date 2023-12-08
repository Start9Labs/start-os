import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import {
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  selector: 'header-menu',
  template: `
    <tui-hosted-dropdown
      [content]="content"
      [tuiDropdownMaxHeight]="9999"
      (click.stop.prevent)="(0)"
      (pointerdown.stop)="(0)"
    >
      <button tuiIconButton appearance="">
        <img style="max-width: 62%" src="assets/img/icon.png" alt="StartOS" />
      </button>
      <ng-template #content>
        <tui-data-list>
          <h3 class="title">StartOS</h3>
          <button tuiOption class="item" (click)="({})">
            <tui-svg src="tuiIconInfo"></tui-svg>
            About this server
          </button>
          <tui-opt-group>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconBookOpen"></tui-svg>
              User Manual
              <tui-svg class="external" src="tuiIconArrowUpRight"></tui-svg>
            </button>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconHeadphones"></tui-svg>
              Contact Support
              <tui-svg class="external" src="tuiIconArrowUpRight"></tui-svg>
            </button>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconDollarSign"></tui-svg>
              Donate to Start9
              <tui-svg class="external" src="tuiIconArrowUpRight"></tui-svg>
            </button>
          </tui-opt-group>
          <tui-opt-group>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconTool"></tui-svg>
              System Rebuild
            </button>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconRefreshCw"></tui-svg>
              Restart
            </button>
            <button tuiOption class="item" (click)="({})">
              <tui-svg src="tuiIconPower"></tui-svg>
              Shutdown
            </button>
          </tui-opt-group>
          <tui-opt-group>
            <button tuiOption class="item" (click)="logout()">
              <tui-svg src="tuiIconLogOut"></tui-svg>
              Logout
            </button>
          </tui-opt-group>
        </tui-data-list>
      </ng-template>
    </tui-hosted-dropdown>
  `,
  styles: [
    `
      .item {
        justify-content: flex-start;
        gap: 0.75rem;
      }

      .title {
        margin: 0;
        padding: 0 0.5rem 0.25rem;
        white-space: nowrap;
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
  ],
})
export class HeaderMenuComponent {
  private readonly api = inject(ApiService)
  private readonly auth = inject(AuthService)

  logout() {
    this.api.logout({}).catch(e => console.error('Failed to log out', e))
    this.auth.setUnverified()
  }
}
