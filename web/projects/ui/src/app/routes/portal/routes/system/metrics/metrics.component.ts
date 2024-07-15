import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiProgress } from '@taiga-ui/kit'
import { CpuComponent } from 'src/app/routes/portal/routes/system/metrics/cpu.component'
import { TemperatureComponent } from 'src/app/routes/portal/routes/system/metrics/temperature.component'
import { MetricComponent } from 'src/app/routes/portal/routes/system/metrics/metric.component'
import { MetricsService } from 'src/app/routes/portal/routes/system/metrics/metrics.service'
import { TimeService } from 'src/app/services/time.service'

@Component({
  standalone: true,
  selector: 'app-metrics',
  template: `
    <section>
      <app-metric class="wide" label="Storage" [style.max-height.%]="85">
        <progress
          tuiProgressBar
          [max]="100"
          [attr.value]="metrics()?.disk?.percentageUsed?.value"
        ></progress>
        <footer>
          <div>
            <span [attr.data-unit]="metrics()?.disk?.used?.unit">
              {{ getValue(metrics()?.disk?.used?.value) }}
            </span>
            Used
          </div>
          <hr />
          <div>
            <span [attr.data-unit]="metrics()?.disk?.available?.unit">
              {{ getValue(metrics()?.disk?.available?.value) }}
            </span>
            Available
          </div>
        </footer>
      </app-metric>
      <app-metric label="CPU">
        <app-cpu [value]="cpu" />
      </app-metric>
      <app-metric label="Memory">
        <label tuiProgressLabel>
          <tui-progress-circle size="l" [max]="100" [value]="memory" />
          {{ metrics()?.memory?.percentageUsed?.value || ' - ' }}%
        </label>
        <footer>
          <div>
            <span [attr.data-unit]="metrics()?.memory?.used?.unit">
              {{ getValue(metrics()?.memory?.used?.value) }}
            </span>
            Used
          </div>
          <hr />
          <div>
            <span [attr.data-unit]="metrics()?.memory?.available?.unit">
              {{ getValue(metrics()?.memory?.available?.value) }}
            </span>
            Available
          </div>
        </footer>
      </app-metric>
      <aside>
        <app-metric label="Uptime" [style.flex]="'unset'">
          <label>
            {{ uptime() }}
            <div>Days : Hrs : Mins : Secs</div>
          </label>
        </app-metric>
        <app-metric label="Temperature">
          <app-temperature [value]="temperature" />
        </app-metric>
      </aside>
    </section>
  `,
  styles: `
    section {
      display: flex;
      gap: 1rem;
      padding: 1rem 1rem 0;
    }

    aside {
      display: flex;
      flex: 1;
      flex-direction: column;
      gap: 1rem;
    }

    footer {
      display: flex;
      white-space: nowrap;
      background: var(--tui-background-neutral-1);
    }

    label {
      margin: auto;
      text-align: center;
      padding: 0.375rem 0;
    }

    progress {
      height: 1.5rem;
      width: 80%;
      margin: auto;
      border-radius: 0;
      clip-path: none;
      mask: linear-gradient(to right, #000 80%, transparent 80%);
      mask-size: 5% 100%;
    }

    hr {
      height: 100%;
      width: 1px;
      margin: 0;
      background: rgba(0, 0, 0, 0.1);
    }

    div {
      display: flex;
      flex-direction: column;
      flex: 1;
      text-align: center;
      text-transform: uppercase;
      color: var(--tui-text-secondary);
      font-size: 0.5rem;
      line-height: 1rem;

      span {
        font-size: 0.75rem;
        font-weight: bold;
        color: var(--tui-text-primary);
        padding-top: 0.4rem;

        &::after {
          content: attr(data-unit);
          font-size: 0.5rem;
          font-weight: normal;
          color: var(--tui-text-secondary);
        }
      }
    }

    :host-context(tui-root._mobile) {
      section {
        min-height: 100%;
        flex-wrap: wrap;
        margin: 0 -2rem -2rem;
      }

      aside {
        order: -1;
        flex-direction: row;
      }

      app-metric {
        min-width: calc(50% - 0.5rem);

        &.wide {
          min-width: 100%;
        }
      }
    }
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiProgress,
    MetricComponent,
    TemperatureComponent,
    CpuComponent,
    AsyncPipe,
  ],
})
export default class SystemMetricsComponent {
  readonly metrics = toSignal(inject(MetricsService))
  readonly uptime = toSignal(inject(TimeService).uptime$)

  get cpu(): number {
    return Number(this.metrics()?.cpu.percentageUsed.value || 0) / 100
  }

  get temperature(): number {
    return Number(this.metrics()?.general.temperature?.value || 0)
  }

  get memory(): number {
    return Number(this.metrics()?.memory?.percentageUsed?.value) || 0
  }

  getValue(value?: string | null): number | string | undefined {
    return value == null ? '-' : Number.parseInt(value)
  }
}
