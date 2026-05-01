import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { NavigationEnd, Router } from '@angular/router'
import { TuiScrollbar } from '@taiga-ui/core'
import { filter, map } from 'rxjs'
import { HELP, HELP_OPEN } from 'src/app/help/help'

@Component({
  selector: '[appAside]',
  template: `
    <tui-scrollbar>
      <div class="g-help" [innerHTML]="help()"></div>
    </tui-scrollbar>
  `,
  styles: `
    :host {
      position: relative;
      width: 20.75rem;
      background: var(--tui-background-base);
      box-shadow:
        inset 0.25rem 0 var(--tui-theme-color),
        0 -0.25rem var(--tui-theme-color);
      border-top-left-radius: var(--tui-radius-xs);
      transition: transform var(--tui-duration);

      &::before {
        content: '';
        position: absolute;
        inset-block-start: 0;
        inset-inline-end: 100%;
        block-size: 1rem;
        inline-size: 1rem;
        pointer-events: none;
        border-top-right-radius: var(--tui-radius-xs);
        box-shadow: 0 -0.25rem var(--tui-theme-color);
      }

      &[inert] {
        transform: translate3d(100%, 0, 0);
      }
    }

    :host-context(tui-root._mobile) {
      position: absolute;
      z-index: 1;
      top: 3rem;
      bottom: 0;
      inset-inline-end: 0;
      animation: tuiFade 0s 1s both;
    }
  `,
  host: { '[attr.inert]': '!open() || null' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiScrollbar],
})
export class Aside {
  protected readonly open = inject(HELP_OPEN)
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
