import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLoaderModule, TuiSvgModule } from '@taiga-ui/core'
import {
  HealthCheckResult,
  HealthResult,
} from 'src/app/services/patch-db/data-model'

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

    return (
      !result ||
      result === HealthResult.Starting ||
      result === HealthResult.Loading
    )
  }

  get icon(): string {
    switch (this.check.result) {
      case HealthResult.Success:
        return 'tuiIconCheckLarge'
      case HealthResult.Failure:
        return 'tuiIconAlertTriangleLarge'
      default:
        return 'tuiIconMinusLarge'
    }
  }

  get color(): string {
    switch (this.check.result) {
      case HealthResult.Success:
        return 'var(--tui-success-fill)'
      case HealthResult.Failure:
        return 'var(--tui-warning-fill)'
      case HealthResult.Starting:
      case HealthResult.Loading:
        return 'var(--tui-primary)'
      default:
        return 'var(--tui-text-02)'
    }
  }

  get message(): string {
    if (!this.check.result) {
      return 'Awaiting result...'
    }

    switch (this.check.result) {
      case HealthResult.Starting:
        return 'Starting...'
      case HealthResult.Success:
        return `Success: ${this.check.message}`
      case HealthResult.Loading:
      case HealthResult.Failure:
        return this.check.message
      default:
        return this.check.result
    }
  }
}
