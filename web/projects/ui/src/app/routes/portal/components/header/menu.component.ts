import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiActiveZoneModule } from '@taiga-ui/cdk'
import {
  TuiDataListModule,
  TuiDialogService,
  TuiDropdownModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import {
  TuiBadgeNotificationModule,
  TuiButtonModule,
  TuiIconModule,
} from '@taiga-ui/experimental'
import { TuiDataListDropdownManagerModule } from '@taiga-ui/kit'
import { RESOURCES } from 'src/app/utils/resources'
import { getMenu } from 'src/app/utils/system-utilities'
import { ABOUT } from './about.component'
import { HeaderConnectionComponent } from './connection.component'

@Component({
  selector: 'header-menu',
  template: `
    <tui-hosted-dropdown
      [content]="content"
      [(open)]="open"
      [tuiDropdownMaxHeight]="9999"
    >
      <button tuiIconButton appearance="">
        <img [style.max-width.%]="50" src="assets/img/icon.png" alt="StartOS" />
      </button>
      <ng-template #content let-zone>
        <tui-data-list tuiDataListDropdownManager [tuiActiveZoneParent]="zone">
          <header-connection class="status">
            <h3 class="title">StartOS</h3>
          </header-connection>
          @for (link of utils; track $index) {
            <a
              tuiOption
              class="item"
              [routerLink]="link.routerLink"
              (click)="open = false"
            >
              <tui-icon [icon]="link.icon" />
              {{ link.name }}
              @if (link.badge(); as badge) {
                <tui-badge-notification>{{ badge }}</tui-badge-notification>
              }
            </a>
          }
          <button
            tuiOption
            class="item"
            tuiDropdownSided
            [tuiDropdown]="dropdown"
            [tuiDropdownOffset]="12"
            [tuiDropdownManual]="false"
          >
            <tui-icon icon="tuiIconHelpCircle" />
            Resources
            <ng-template #dropdown>
              <tui-data-list [tuiActiveZoneParent]="zone">
                <button tuiOption class="item" (click)="about()">
                  <tui-icon icon="tuiIconInfo" />
                  About this server
                </button>
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
                    <tui-icon class="external" icon="tuiIconExternalLink" />
                  </a>
                }
              </tui-data-list>
            </ng-template>
          </button>
        </tui-data-list>
      </ng-template>
    </tui-hosted-dropdown>
  `,
  styles: [
    `
      tui-icon {
        font-size: 1rem;
      }

      tui-hosted-dropdown {
        margin: 0 -0.5rem;

        [tuiIconButton] {
          height: calc(var(--tui-height-m) + 0.375rem);
          width: calc(var(--tui-height-m) + 0.625rem);
        }
      }

      .item {
        justify-content: flex-start;
        gap: 0.75rem;

        ::ng-deep tui-svg {
          margin-left: auto;
        }
      }

      .status {
        display: flex !important;
        font-size: 0;
        padding: 0 0.5rem;
        height: 2rem;
        width: 13rem;
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
    RouterLink,
    TuiBadgeNotificationModule,
    TuiDropdownModule,
    TuiDataListDropdownManagerModule,
    TuiActiveZoneModule,
  ],
})
export class HeaderMenuComponent {
  private readonly dialogs = inject(TuiDialogService)

  open = false

  readonly utils = getMenu()
  readonly links = RESOURCES

  about() {
    this.dialogs.open(ABOUT, { label: 'About this server' }).subscribe()
  }
}
