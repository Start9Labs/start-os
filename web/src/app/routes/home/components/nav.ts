import { KeyValuePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router, RouterLink, RouterLinkActive } from '@angular/router'
import { TuiButton, TuiScrollbar } from '@taiga-ui/core'
import { MENU } from 'src/app/routes/home/components/menu'
import { AuthService } from 'src/app/services/auth.service'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  selector: 'nav',
  template: `
    <tui-scrollbar>
      @for (item of routes | keyvalue: asIs; track $index) {
        <span>{{ item.key }}</span>
        @for (route of item.value; track $index) {
          <a
            tuiButton
            size="s"
            appearance="flat-grayscale"
            routerLinkActive="active"
            [iconStart]="route.icon"
            [routerLink]="route.link"
          >
            {{ route.name }}
          </a>
        }
        @if (!$last) {
          <hr />
        }
      }
    </tui-scrollbar>
    <button
      tuiButton
      iconStart="@tui.log-out"
      appearance="neutral"
      size="s"
      (click)="logout()"
    >
      Logout
    </button>
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      background: var(--tui-background-neutral-1);
      backdrop-filter: blur(1rem);
      z-index: 1;
      overflow: hidden;
      transition: transform var(--tui-duration);
    }

    tui-scrollbar {
      flex: 1;
      padding-bottom: 1rem;
    }

    span {
      display: block;
      padding: 0.5rem 1rem;
      font: var(--tui-font-text-s);
      color: var(--tui-text-secondary);
      text-transform: uppercase;

      &:first-child {
        margin-top: 1rem;
      }
    }

    a {
      display: flex;
      justify-content: start;
      margin: 0 0.5rem;

      &.active {
        background: var(--tui-background-neutral-1);
      }
    }

    hr {
      height: 1px;
      border: none;
      background: var(--tui-border-normal);
      margin: 0.5rem 0;
    }

    button {
      width: 100%;
      border-radius: 0;
      justify-content: flex-start;
    }

    :host-context(tui-root._mobile) {
      position: absolute;
      top: 3.5rem;
      width: 14rem;
      bottom: 0;
      inset-inline-start: 0;

      &:not(:focus-within, ._expanded) {
        transform: translate3d(-100%, 0, 0);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiScrollbar,
    TuiButton,
    RouterLink,
    KeyValuePipe,
    RouterLinkActive,
  ],
  host: {
    '[class._expanded]': 'sidebars.start()',
    '(document:click)': 'sidebars.start.set(false)',
    '(mousedown.prevent)': '0',
  },
})
export class Nav {
  private readonly service = inject(AuthService)
  private readonly router = inject(Router)

  protected readonly sidebars = inject(SidebarService)
  protected readonly routes = MENU

  protected logout(): void {
    this.service.authenticated.set(false)
    this.router.navigate(['.'])
  }

  protected asIs(): number {
    return 0
  }
}
