import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router, RouterModule } from '@angular/router'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { NavigationService } from '../services/navigation.service'

@Component({
  selector: 'nav[appNavigation]',
  template: `
    <a
      class="tab"
      routerLink="desktop"
      routerLinkActive="tab_active"
      [routerLinkActiveOptions]="{ exact: true }"
    >
      <tui-icon icon="tuiIconHome" class="icon" />
    </a>
    @for (tab of tabs$ | async; track tab) {
      <a
        #rla="routerLinkActive"
        class="tab"
        routerLinkActive="tab_active"
        [routerLink]="tab.routerLink"
      >
        @if (tab.icon.startsWith('tuiIcon')) {
          <tui-icon class="icon" [icon]="tab.icon" />
        } @else {
          <img class="icon" [src]="tab.icon" [alt]="tab.title" />
        }
        <button
          tuiIconButton
          size="xs"
          iconLeft="tuiIconClose"
          appearance="icon"
          class="close"
          (click.stop.prevent)="removeTab(tab.routerLink, rla.isActive)"
        >
          Close
        </button>
      </a>
    }
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        @include scrollbar-hidden;

        height: 3rem;
        display: flex;
        overflow: auto;
        // TODO: Theme
        background: rgb(97 95 95 / 84%);
      }

      .tab {
        position: relative;
        display: flex;
        flex-shrink: 0;
        align-items: center;
        justify-content: center;
        width: 7.5rem;

        &_active {
          position: sticky;
          left: 0;
          right: 0;
          z-index: 1;
          // TODO: Theme
          background: #373a3f;
        }
      }

      .icon {
        width: 2rem;
        height: 2rem;
        border-radius: 100%;
        color: var(--tui-base-08);
      }

      .close {
        position: absolute;
        top: 0;
        right: 0;
      }
    `,
  ],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, RouterModule, TuiButtonModule, TuiIconModule],
})
export class NavigationComponent {
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)

  readonly tabs$ = this.navigation.getTabs()

  removeTab(routerLink: string, active: boolean) {
    this.navigation.removeTab(routerLink)

    if (active) this.router.navigate(['/portal/desktop'])
  }
}
