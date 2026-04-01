import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NavigationEnd, Router } from '@angular/router'
import { TuiScrollbar } from '@taiga-ui/core'
import { filter, map } from 'rxjs'
import { HELP } from 'src/app/help/help'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'aside',
  template: `
    <tui-scrollbar>
      <div class="g-help" [innerHTML]="help()"></div>
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
  imports: [TuiScrollbar],
})
export class Aside {
  protected readonly sidebars = inject(SidebarService)
  protected readonly data = inject(HELP)
  private readonly router = inject(Router)
  protected readonly help = toSignal(
    this.router.events.pipe(
      filter(e => e instanceof NavigationEnd),
      map(({ urlAfterRedirects }) => {
        const path = urlAfterRedirects.split('?')[0]
        return this.data[path] ?? this.data[this.normalize(path)]
      }),
    ),
  )

  /** Strip dynamic route params: /profiles/:interface/schedule → /profiles/schedule */
  private normalize(path: string): string {
    return path.replace(/^\/profiles\/[^/]+\/schedule/, '/profiles/schedule')
  }
}
