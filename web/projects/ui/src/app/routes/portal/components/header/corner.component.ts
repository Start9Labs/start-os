import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiSidebar } from '@taiga-ui/addon-mobile'
import { TuiLet } from '@taiga-ui/cdk'
import {
  TUI_ANIMATIONS_SPEED,
  TuiButton,
  tuiFadeIn,
  tuiScaleIn,
  tuiToAnimationOptions,
  tuiWidthCollapse,
} from '@taiga-ui/core'
import { TuiBadgedContent, TuiBadgeNotification } from '@taiga-ui/kit'
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
            [iconStart]="item.icon"
            [routerLink]="item.routerLink"
            [style.color]="'var(--tui-text-primary)'"
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
    TuiBadgeNotification,
    TuiBadgedContent,
    TuiButton,
    TuiLet,
    TuiSidebar,
    RouterLink,
  ],
})
export class HeaderCornerComponent {
  readonly animation = tuiToAnimationOptions(inject(TUI_ANIMATIONS_SPEED))
  readonly utils = getMenu()
}
