import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'

@Component({
  standalone: true,
  selector: 'tr[healthCheck]',
  template: `
    <td>{{ healthCheck.name }}</td>
    <td>
      <span>
        @if (loading) {
          <tui-loader size="m" />
        } @else {
          <tui-icon [icon]="icon" [style.color]="color" />
        }
        {{ message }}
      </span>
    </td>
  `,
  styles: [
    `
      span {
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }

      :host-context(tui-root._mobile) {
        display: flex;
        flex-direction: column;

        td:first-child {
          font-weight: bold;
          padding-bottom: 0;
        }

        td:last-child {
          color: var(--tui-text-secondary);
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLoader, TuiIcon],
})
export class ServiceHealthCheckComponent {
  @Input({ required: true })
  healthCheck!: T.NamedHealthCheckResult

  get loading(): boolean {
    const { result } = this.healthCheck

    return !result || result === 'starting' || result === 'loading'
  }

  get icon(): string {
    switch (this.healthCheck.result) {
      case 'success':
        return '@tui.check'
      case 'failure':
        return '@tui.triangle-alert'
      default:
        return '@tui.minus'
    }
  }

  get color(): string {
    switch (this.healthCheck.result) {
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
  }

  get message(): string {
    if (!this.healthCheck.result) {
      return 'Awaiting result...'
    }

    switch (this.healthCheck.result) {
      case 'starting':
        return 'Starting...'
      case 'success':
        return `Success: ${this.healthCheck.message}`
      case 'loading':
      case 'failure':
        return this.healthCheck.message
      // disabled
      default:
        return this.healthCheck.result
    }
  }
}
