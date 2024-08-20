import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  TuiButton,
  TuiDataList,
  TuiDialogService,
  TuiDropdown,
  TuiIcon,
} from '@taiga-ui/core'
import { TuiBadgeNotification, TuiDataListDropdownManager } from '@taiga-ui/kit'
import { RESOURCES } from 'src/app/utils/resources'
import { getMenu } from 'src/app/utils/system-utilities'
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
      <img [style.max-width.%]="50" src="assets/img/icon.png" alt="StartOS" />
    </button>
    <ng-template #content>
      <tui-data-list tuiDataListDropdownManager [style.width.rem]="13">
        @for (link of utils; track $index) {
          <a
            tuiOption
            class="item"
            [iconStart]="link.icon"
            [routerLink]="link.routerLink"
            (click)="open = false"
          >
            {{ link.name }}
            @if (link.badge(); as badge) {
              <tui-badge-notification>{{ badge }}</tui-badge-notification>
            }
          </a>
          @if (!$index || $index === 3 || $index === 5) {
            <hr />
          }
        }
        <hr />
        <button
          tuiOption
          class="item"
          tuiDropdownSided
          iconStart="@tui.circle-help"
          iconEnd="@tui.chevron-right"
          [tuiDropdown]="dropdown"
          [tuiDropdownOffset]="12"
          [tuiDropdownManual]="false"
        >
          Resources
          <ng-template #dropdown>
            <tui-data-list>
              <button
                tuiOption
                iconStart="@tui.info"
                class="item"
                (click)="about()"
              >
                About this server
              </button>
              <hr />
              @for (link of links; track $index) {
                <a
                  tuiOption
                  class="item"
                  target="_blank"
                  rel="noreferrer"
                  iconEnd="@tui.external-link"
                  [iconStart]="link.icon"
                  [href]="link.href"
                >
                  {{ link.name }}
                </a>
              }
            </tui-data-list>
          </ng-template>
        </button>
      </tui-data-list>
    </ng-template>
  `,
  styles: [
    `
      :host {
        margin: 0 -0.5rem;
      }

      [tuiIconButton] {
        height: calc(var(--tui-height-m) + 0.25rem);
        width: calc(var(--tui-height-m) + 0.625rem);
      }

      .item {
        justify-content: flex-start;
        gap: 0.5rem;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiDropdown,
    TuiDataList,
    TuiButton,
    TuiIcon,
    RouterLink,
    TuiBadgeNotification,
    TuiDropdown,
    TuiDataListDropdownManager,
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
