import { NgTemplateOutlet } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiScrollbar } from '@taiga-ui/core'
import { HelpService } from 'src/app/services/help.service'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'aside',
  template: `
    <tui-scrollbar>
      <ng-container *ngTemplateOutlet="help.content()" />
    </tui-scrollbar>
  `,
  styles: `
    :host {
      background: var(--tui-background-neutral-1);
      width: 20rem;
      overflow: hidden;
      backdrop-filter: blur(1rem);
      transition: transform var(--tui-duration);

      &[inert] {
        transform: translate3d(100%, 0, 0);
      }

      ::ng-deep tui-expand {
        box-shadow: none;
      }
    }

    :host-context(tui-root._mobile) {
      position: absolute;
      top: 3.5rem;
      bottom: 0;
      inset-inline-end: 0;
      animation: tuiFade 0s 1s both;
    }
  `,
  host: { '[attr.inert]': '!sidebars.end() || null' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [NgTemplateOutlet, TuiScrollbar],
})
export class Aside {
  protected readonly sidebars = inject(SidebarService)
  protected readonly help = inject(HelpService)
}
