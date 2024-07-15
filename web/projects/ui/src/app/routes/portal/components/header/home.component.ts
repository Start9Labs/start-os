import { TuiIcon } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  standalone: true,
  selector: 'a[headerHome]',
  template: `
    <ng-content />
    <tui-icon icon="/assets/img/icons/home.svg" [style.font-size.rem]="2" />
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        display: flex;
        align-items: center;
        justify-content: center;
        height: 100%;
        padding: 0 2.5rem 0 1rem;
        margin: 0 !important;

        --clip-path: polygon(
          calc(100% - 1.75rem) 0%,
          calc(100% - 0.875rem) 50%,
          calc(100% - 1.75rem) 100%,
          0% 100%,
          0% 0%
        );

        &.active {
          --clip-path: polygon(
            calc(100% - 1.75rem) 0%,
            calc(100% - 0.875rem) 50%,
            100% 100%,
            0% 100%,
            0% 0%
          );
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon],
})
export class HeaderHomeComponent {}
