import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { TuiScrollbar } from '@taiga-ui/core'
import { Header } from 'src/app/routes/home/components/header'
import { Nav } from 'src/app/routes/home/components/nav'

@Component({
  selector: 'app-outlet',
  template: `
    <header></header>
    <nav></nav>
    <main>
      <tui-scrollbar>
        <router-outlet />
      </tui-scrollbar>
    </main>
  `,
  styles: `
    :host {
      height: 100%;
      display: grid;
      grid-template: 3.5rem 1fr / 14rem 1fr;
      overflow: hidden;
      transition: grid-template var(--tui-duration);
    }

    main {
      isolation: isolate;
      overflow: hidden;
      padding: 1rem;
    }

    tui-scrollbar {
      max-width: 50rem;
      margin: 0 auto;
      border-radius: var(--tui-radius-s);

      ::ng-deep > tui-scroll-controls {
        top: var(--tui-height-m);
      }
    }

    :host-context(tui-root._mobile) {
      grid-template: 3.5rem 1fr / 1fr;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Header, Nav, RouterOutlet, TuiScrollbar],
})
export class Outlet {}
