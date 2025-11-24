import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  inject,
  viewChild,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Router, RouterOutlet } from '@angular/router'
import { TuiScrollbar } from '@taiga-ui/core'
import { Aside } from 'src/app/routes/home/components/aside'
import { Header } from 'src/app/routes/home/components/header'
import { Nav } from 'src/app/routes/home/components/nav'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'app-outlet',
  template: `
    <header></header>
    <nav></nav>
    <main>
      <tui-scrollbar><router-outlet /></tui-scrollbar>
    </main>
    <aside inert></aside>
  `,
  styles: `
    :host {
      height: 100%;
      display: grid;
      grid-template: 3.5rem 1fr / 14rem 1fr 20rem;
      overflow: hidden;
      transition: grid-template var(--tui-duration);

      &:has(aside[inert]) {
        grid-template: 3.5rem 1fr / 14rem 1fr 0;
      }
    }

    main {
      isolation: isolate;
      overflow: hidden;
    }

    tui-scrollbar {
      height: 100%;

      ::ng-deep > .t-content {
        height: 100%;
        width: auto;
      }
    }

    router-outlet + ::ng-deep ng-component {
      display: block;
      margin: 1rem 1.5rem;
    }

    :host-context(tui-root._mobile) {
      grid-template: 3.5rem 1fr / 1fr;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Header, Nav, Aside, RouterOutlet, TuiScrollbar],
})
export class Outlet {
  protected readonly scrollbar = viewChild(TuiScrollbar, { read: ElementRef })
  protected readonly _ = inject(Router)
    .events.pipe(takeUntilDestroyed())
    .subscribe(() => {
      this.scrollbar()?.nativeElement.scrollTo({ top: 0 })
    })
}
