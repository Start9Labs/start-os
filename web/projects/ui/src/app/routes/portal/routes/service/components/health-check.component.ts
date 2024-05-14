import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { HealthCheckResult } from '@startos'
import { TuiLoaderModule, TuiSvgModule } from '@taiga-ui/core'

@Component({
  selector: 'service-health-check',
  template: `
    <tui-loader
      *ngIf="loading; else svg"
      [class.tui-skeleton]="!connected"
      [inheritColor]="!check.result"
    ></tui-loader>
    <ng-template #svg>
      <tui-svg
        [src]="icon"
        [class.tui-skeleton]="!connected"
        [style.color]="color"
      ></tui-svg>
    </ng-template>
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
  imports: [CommonModule, TuiLoaderModule, TuiSvgModule],
})
export class ServiceHealthCheckComponent {
  @Input({ required: true })
  check!: HealthCheckResult

  @Input()
  connected = false

  get loading(): boolean {
    const { result } = this.check

    return !result || result === 'starting' || result === 'loading'
  }

  get icon(): string {
    switch (this.check.result) {
      case 'success':
        return 'tuiIconCheckLarge'
      case 'failure':
        return 'tuiIconAlertTriangleLarge'
      default:
        return 'tuiIconMinusLarge'
    }
  }

  get color(): string {
    switch (this.check.result) {
      case 'success':
        return 'var(--tui-success-fill)'
      case 'failure':
        return 'var(--tui-warning-fill)'
      case 'starting':
      case 'loading':
        return 'var(--tui-primary)'
      // disabled
      default:
        return 'var(--tui-text-02)'
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
