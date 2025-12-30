import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'

@Component({
  selector: 'tr[healthCheck]',
  template: `
    <td class="name">{{ healthCheck().name }}</td>
    <td>
      <span>
        @if (loading()) {
          <tui-loader size="m" />
        } @else {
          <tui-icon [icon]="icon()" [style.color]="color()" />
        }
        {{ message() }}
      </span>
    </td>
  `,
  styles: `
    span {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .name {
      width: 9.5rem;
    }

    :host-context(tui-root._mobile) {
      display: flex;
      flex-direction: column;

      td {
        width: 100%;

        &:first-child {
          font-weight: bold;
          padding-bottom: 0;
        }

        &:last-child {
          color: var(--tui-text-secondary);
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader, TuiIcon],
})
export class ServiceHealthCheckComponent {
  private readonly i18n = inject(i18nPipe)

  readonly healthCheck = input.required<T.NamedHealthCheckResult>()

  readonly loading = computed(
    ({ result } = this.healthCheck()) =>
      !result || result === 'starting' || result === 'loading',
  )

  readonly icon = computed(() => {
    switch (this.healthCheck().result) {
      case 'success':
        return '@tui.check'
      case 'failure':
        return '@tui.triangle-alert'
      default:
        return '@tui.minus'
    }
  })

  readonly color = computed(() => {
    switch (this.healthCheck().result) {
      case 'success':
        return 'var(--tui-status-positive)'
      case 'failure':
        return 'var(--tui-status-warning)'
      case 'starting':
      case 'loading':
        return 'var(--tui-background-accent-1)'
      // disabled
      default:
        return 'var(--tui-text-secondary)'
    }
  })

  readonly message = computed(({ result, message } = this.healthCheck()) => {
    if (!result) {
      return this.i18n.transform('Awaiting result')!
    }

    switch (result) {
      case 'starting':
        return this.i18n.transform('Starting')!
      case 'success':
        return `${this.i18n.transform('Success')}: ${message || 'health check passing'}`
      case 'loading':
      case 'failure':
      case 'disabled':
        return message || result
    }
  })
}
