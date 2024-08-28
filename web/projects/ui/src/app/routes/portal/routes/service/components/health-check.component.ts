import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'

@Component({
  selector: 'service-health-check',
  template: `
    @if (loading) {
      <tui-loader
        [class.tui-skeleton]="!connected"
        [inheritColor]="!check.result"
      />
    } @else {
      <tui-icon
        [icon]="icon"
        [class.tui-skeleton]="!connected"
        [style.color]="color"
      />
    }
    <div>
      <strong [class.tui-skeleton]="!connected">{{ check.name }}</strong>
      <div [class.tui-skeleton]="!connected" [style.color]="color">
        {{ message }}
      </div>
    </div>
  `,
  styles: [
    `
      :first-letter {
        text-transform: uppercase;
      }

      tui-loader {
        width: 1.5rem;
        height: 1.5rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiLoader, TuiIcon],
})
export class ServiceHealthCheckComponent {
  @Input({ required: true })
  check!: T.NamedHealthCheckResult

  @Input()
  connected = false

  get loading(): boolean {
    const { result } = this.check

    return !result || result === 'starting' || result === 'loading'
  }

  get icon(): string {
    switch (this.check.result) {
      case 'success':
        return '@tui.check'
      case 'failure':
        return '@tui.triangle-alert'
      default:
        return '@tui.minus'
    }
  }

  get color(): string {
    switch (this.check.result) {
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
    if (!this.check.result) {
      return 'Awaiting result...'
    }

    switch (this.check.result) {
      case 'starting':
        return 'Starting...'
      case 'success':
        return `Success: ${this.check.message}`
      case 'loading':
      case 'failure':
        return this.check.message
      // disabled
      default:
        return this.check.result
    }
  }
}
