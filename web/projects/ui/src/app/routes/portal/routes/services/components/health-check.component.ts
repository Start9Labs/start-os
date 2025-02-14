import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiLoader, TuiTitle } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'

@Component({
  selector: 'service-health-check',
  template: `
    @if (loading) {
      <tui-loader [tuiSkeleton]="!connected" [inheritColor]="!check.result" />
    } @else {
      <tui-icon
        [icon]="icon"
        [tuiSkeleton]="!connected"
        [style.color]="color"
      />
    }
    <span tuiTitle>
      <strong [tuiSkeleton]="!connected && 2">
        {{ connected ? check.name : '' }}
      </strong>
      <span tuiSubtitle [tuiSkeleton]="!connected && 3" [style.color]="color">
        {{ connected ? message : '' }}
      </span>
    </span>
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
  hostDirectives: [TuiCell],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiLoader, TuiIcon, TuiTitle, TuiSkeleton],
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
