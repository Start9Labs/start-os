import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import { TuiLetModule } from '@taiga-ui/cdk'
import {
  TUI_ANIMATION_OPTIONS,
  tuiFadeIn,
  tuiScaleIn,
  tuiWidthCollapse,
} from '@taiga-ui/core'
import {
  TuiBadgedContentModule,
  TuiBadgeNotificationModule,
  TuiButtonModule,
} from '@taiga-ui/experimental'
import { SidebarDirective } from 'src/app/components/sidebar-host.component'
import { getMenu } from 'src/app/utils/system-utilities'
import { HeaderMenuComponent } from './menu.component'

@Component({
  standalone: true,
  selector: 'header-corner',
  template: `
    <ng-content />
    @for (item of utils; track $index) {
      @if (item.badge(); as badge) {
        <tui-badged-content
          [style.--tui-radius.%]="50"
          [@tuiFadeIn]="animation"
          [@tuiWidthCollapse]="animation"
          [@tuiScaleIn]="animation"
        >
          <tui-badge-notification tuiSlot="top" size="s">
            {{ badge }}
          </tui-badge-notification>
          <a
            tuiIconButton
            appearance="icon"
            size="s"
            [iconLeft]="item.icon"
            [routerLink]="item.routerLink"
            [style.color]="'var(--tui-text-01)'"
          >
            {{ item.name }}
          </a>
        </tui-badged-content>
      }
    }
    <header-menu></header-menu>
  `,
  styles: [
    `
      :host {
        display: flex;
        align-items: center;
        gap: 0.25rem;
        padding: 0 0.5rem 0 1.75rem;
        --clip-path: polygon(0% 0%, 100% 0%, 100% 100%, 1.75rem 100%);
      }

      :host-context(tui-root._mobile) tui-badged-content {
        display: none;
      }
    `,
  ],
  animations: [tuiFadeIn, tuiWidthCollapse, tuiScaleIn],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    HeaderMenuComponent,
    SidebarDirective,
    TuiBadgeNotificationModule,
    TuiBadgedContentModule,
    TuiButtonModule,
    TuiLetModule,
    TuiSidebarModule,
    RouterLink,
  ],
})
export class HeaderCornerComponent {
  readonly animation = inject(TUI_ANIMATION_OPTIONS)
  readonly utils = getMenu()
}
