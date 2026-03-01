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
import { Aside } from 'src/app/components/aside'
import { Header } from 'src/app/components/header'
import { Nav } from 'src/app/components/nav'
import { SystemService } from 'src/app/services/system.service'

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
      display: flex;
      flex-direction: column;
      margin: 1rem 1.5rem;

      > header[tuiHeader] {
        position: sticky;
        inset-inline-start: 1.5rem;
        max-width: calc(100vw - 3rem);

        + tui-tabs {
          flex-shrink: 0;
          margin-block: 1rem;
        }
      }

      &::after {
        content: '';
        block-size: 5rem;
      }
    }

    :host-context(tui-root._mobile) {
      grid-template: 3.5rem 1fr / 1fr;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Header, Nav, Aside, RouterOutlet, TuiScrollbar],
})
export class App {
  protected readonly scrollbar = viewChild(TuiScrollbar, { read: ElementRef })
  protected readonly _ = inject(Router)
    .events.pipe(takeUntilDestroyed())
    .subscribe(() => {
      this.scrollbar()?.nativeElement.scrollTo({ top: 0 })
    })

  constructor() {
    inject(SystemService).init()
  }
}
