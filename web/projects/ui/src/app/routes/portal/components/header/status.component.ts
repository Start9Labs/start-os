import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'
import { STATUS } from 'src/app/services/status.service'

@Component({
  standalone: true,
  selector: 'header-status',
  template: `
    <span>
      <!-- data-status is used to display color indicator in the header through :has() -->
      <tui-icon
        [icon]="status().icon"
        [style.color]="status().color"
        [style.font-size.em]="1.5"
        [attr.data-status]="status().status"
      />
    </span>
    <span>{{ status().message }}</span>
  `,
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      :host {
        @include transition(all);
        display: grid;
        grid-template-columns: 1.75rem 1fr;
        align-items: center;
        padding: 0 1rem;
        margin-inline-start: var(--bumper);

        &._connected {
          grid-template-columns: 0fr 0fr;
          padding: 0;
          margin: 0;
        }

        > * {
          overflow: hidden;
        }
      }

      :host-context(tui-root._mobile) {
        display: none;
      }
    `,
  ],
  host: { '[class._connected]': 'status().status === "success"' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, AsyncPipe],
})
export class HeaderStatusComponent {
  readonly status = inject(STATUS)
}
