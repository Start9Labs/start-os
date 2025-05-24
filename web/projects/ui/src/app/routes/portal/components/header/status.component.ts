import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiIcon } from '@taiga-ui/core'
import { STATUS } from 'src/app/services/status.service'

@Component({
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
    <span>{{ status().message | i18n }}</span>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      @include taiga.transition(all);
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
  host: { '[class._connected]': 'status().status === "success"' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, i18nPipe],
})
export class HeaderStatusComponent {
  readonly status = inject(STATUS)
}
