import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiIconModule } from '@taiga-ui/experimental'
import { SYSTEM_UTILITIES } from 'src/app/apps/portal/constants/system-utilities'

@Component({
  standalone: true,
  selector: 'app-utilities',
  template: `
    <ng-content />
    <div class="links">
      @for (item of items; track $index) {
        <a class="link" [routerLink]="item.routerLink">
          <tui-icon [icon]="item.icon" />
          {{ item.title }}
        </a>
      }
    </div>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      --clip-path: polygon(
        0 2rem,
        1.25rem 0,
        8.75rem 0,
        calc(10rem + 0.1em) calc(2rem - 0.1em),
        calc(100% - 1.25rem) 2rem,
        100% 4rem,
        100% calc(100% - 2rem),
        calc(100% - 1.25rem) 100%,
        1.25rem 100%,
        0 calc(100% - 2rem)
      );
    }

    .links {
      display: grid;
      grid-template: 1fr 1fr / 1fr 1fr 1fr;
      gap: 0.75rem;
      padding: 1.5rem;
      font-size: min(0.75rem, 1.25vw);
    }

    .link {
      @include transition(background);
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      aspect-ratio: 1/1;
      border-radius: 0.25rem;
      border: 1px solid var(--tui-clear);

      tui-icon {
        width: 50%;
        height: 50%;
      }

      &:hover {
        background: var(--tui-clear);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIconModule, RouterLink],
})
export class UtilitiesComponent {
  readonly items = Object.keys(SYSTEM_UTILITIES)
    .filter(key => key !== '/portal/system/notifications')
    .map(key => ({
      ...SYSTEM_UTILITIES[key],
      routerLink: key,
    }))
}
