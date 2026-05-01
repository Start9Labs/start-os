import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  inject,
  signal,
  viewChild,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Router, RouterOutlet } from '@angular/router'
import { TUI_DARK_MODE, TuiScrollbar } from '@taiga-ui/core'
import { TuiNavigation } from '@taiga-ui/layout'
import { Aside } from 'src/app/components/aside'
import { Header } from 'src/app/components/header'
import { Nav } from 'src/app/components/nav'
import { SystemService } from 'src/app/services/system.service'

@Component({
  selector: 'app-outlet',
  template: `
    <header tuiNavigationHeader [attr.tuiTheme]="dark() ? 'dark' : null">
      <app-header />
    </header>
    <aside
      class="_expanded"
      [class._expanded]="open()"
      [tuiNavigationAside]="open()"
      [attr.tuiTheme]="dark() ? 'dark' : null"
    >
      <nav appNav></nav>
      <footer>
        <button
          tuiAsideItem
          type="button"
          [iconStart]="open() ? '@tui.chevron-left' : '@tui.chevron-right'"
          (click)="open.set(!open())"
        >
          {{ open() ? 'Collapse' : 'Expand' }}
        </button>
      </footer>
    </aside>
    <main>
      <tui-scrollbar><router-outlet /></tui-scrollbar>
    </main>
    <aside appAside inert></aside>
  `,
  styles: `
    :host {
      height: 100%;
      display: grid;
      grid-template: 3rem 1fr / min-content 1fr 20.75rem;
      overflow: hidden;
      transition: grid-template var(--tui-duration);

      &:has(aside[inert]) {
        grid-template: 3rem 1fr / min-content 1fr 0;
      }

      header {
        grid-column: span 3;

        &::before {
          clip-path: inset(0 0 0 2rem);
        }
      }

      // TODO: Remove after Taiga UI 5.5.0
      aside {
        min-block-size: 0;
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
        flex-shrink: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [Header, Nav, Aside, RouterOutlet, TuiScrollbar, TuiNavigation],
})
export class App {
  protected readonly dark = inject(TUI_DARK_MODE)
  protected readonly open = signal(true)
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
