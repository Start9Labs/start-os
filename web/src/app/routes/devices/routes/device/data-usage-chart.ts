import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  input,
  signal,
} from '@angular/core'
import {
  TUI_ALWAYS_DASHED,
  TuiAxes,
  TuiLineChart,
} from '@taiga-ui/addon-charts'
import { TuiButton, TuiDataList, TuiDropdown, TuiLoader } from '@taiga-ui/core'
import type { TuiPoint } from '@taiga-ui/core'
import { TuiChevron, TuiSkeleton } from '@taiga-ui/kit'
import {
  DATA_USAGE_PERIOD_LABELS,
  DataUsagePeriod,
  DataUsagePoint,
} from '../../utils'
import { DevicesService } from '../../service'

@Component({
  selector: 'app-data-usage-chart',
  template: `
    <header>
      <span [tuiSkeleton]="loading()">Data Usage</span>
      <button
        tuiButton
        tuiChevron
        size="s"
        appearance="secondary-grayscale"
        tuiDropdown
        tuiDropdownAuto
        [tuiSkeleton]="loading()"
      >
        {{ periodLabels[selectedPeriod()] }}
        <tui-data-list *tuiDropdown="let close" size="s" (click)="close()">
          @for (period of periods; track period) {
            <button
              tuiOption
              type="button"
              [value]="period"
              (click)="selectedPeriod.set(period)"
            >
              {{ periodLabels[period] }}
            </button>
          }
        </tui-data-list>
      </button>
    </header>
    @if (loading() || initialLoading()) {
      <div class="chart-legend">
        <span>Placeholder</span>
        <span>Placeholder</span>
      </div>
      <div class="chart-loading">
        <tui-loader />
      </div>
    } @else {
      <div class="chart-legend">
        <span class="legend-item download">
          <span class="legend-dot"></span>
          Download
        </span>
        <span class="legend-item upload">
          <span class="legend-dot"></span>
          Upload
        </span>
      </div>
      <tui-axes
        [axisXLabels]="xLabels()"
        [axisYSecondaryLabels]="yLabels()"
        [horizontalLines]="3"
        [horizontalLinesHandler]="lines"
      >
        @for (line of chartLines(); track $index) {
          <tui-line-chart
            class="line"
            [smoothingFactor]="10"
            [value]="line"
            [width]="400"
            [height]="160"
            [x]="0"
            [y]="0"
          />
        }
      </tui-axes>
    }
  `,
  styles: `
    :host {
      display: block;
      padding: 0.5rem 0.75rem;
      border-radius: var(--tui-radius-xs);
      background: var(--tui-background-neutral-1);
    }

    header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      font-weight: bold;
      margin-bottom: 0.5rem;
    }

    tui-axes {
      height: 10rem;

      .line {
        position: absolute;
        color: #10b981;

        &:first-child {
          color: #3b82f6;
        }
      }
    }

    .chart-loading {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 10rem;
    }

    .chart-legend {
      display: flex;
      gap: 1rem;
      margin-bottom: 0.5rem;
      font-size: 0.75rem;

      .legend-item {
        display: flex;
        align-items: center;
        gap: 0.25rem;
      }

      .legend-dot {
        width: 0.5rem;
        height: 0.5rem;
        border-radius: 50%;
      }

      .download .legend-dot {
        background: #3b82f6;
      }

      .upload .legend-dot {
        background: #10b981;
      }
    }
  `,
  imports: [
    TuiAxes,
    TuiButton,
    TuiChevron,
    TuiDataList,
    TuiDropdown,
    TuiLineChart,
    TuiLoader,
    TuiSkeleton,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DataUsageChart {
  readonly mac = input.required<string>()
  readonly service = input.required<DevicesService>()
  readonly loading = input(false)

  protected readonly lines = TUI_ALWAYS_DASHED
  protected readonly periodLabels = DATA_USAGE_PERIOD_LABELS
  protected readonly periods: DataUsagePeriod[] = [
    'day',
    'week',
    'month',
    '3months',
  ]

  protected readonly selectedPeriod = signal<DataUsagePeriod>('week')
  protected readonly initialLoading = signal(true)
  protected readonly dataPoints = signal<DataUsagePoint[]>([])

  // Convert data to line chart format using absolute coordinates
  protected readonly chartLines = computed((): TuiPoint[][] => {
    const points = this.dataPoints()
    if (points.length < 2) return []

    const width = 400
    const height = 160
    const len = points.length - 1

    // Find max value for scaling
    const maxBytes = Math.max(
      ...points.map(p => Math.max(p.download, p.upload)),
    )
    const scale = maxBytes > 0 ? height / maxBytes : 1

    const downloadLine: TuiPoint[] = points.map(
      (p, i): TuiPoint => [(i / len) * width, p.download * scale],
    )

    const uploadLine: TuiPoint[] = points.map(
      (p, i): TuiPoint => [(i / len) * width, p.upload * scale],
    )

    return [downloadLine, uploadLine]
  })

  protected readonly yMax = computed((): number => {
    const points = this.dataPoints()
    if (points.length === 0) return 1

    const maxBytes = Math.max(
      ...points.map(p => Math.max(p.download, p.upload)),
    )
    const maxGb = maxBytes / (1024 * 1024 * 1024)
    return Math.max(0.5, Math.ceil(maxGb * 2) / 2)
  })

  protected readonly yLabels = computed(() => {
    const max = this.yMax()
    const step = max / 3
    return [
      '0',
      `${step.toFixed(1)}GB`,
      `${(step * 2).toFixed(1)}GB`,
      `${max.toFixed(1)}GB`,
    ]
  })

  protected readonly xLabels = computed(() => {
    const period = this.selectedPeriod()
    switch (period) {
      case 'day':
        return ['00:00', '06:00', '12:00', '18:00', '24:00']
      case 'week':
        return ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
      case 'month':
        return ['Week 1', 'Week 2', 'Week 3', 'Week 4']
      case '3months':
        return ['Month 1', 'Month 2', 'Month 3']
    }
  })

  constructor() {
    effect(() => {
      const mac = this.mac()
      const period = this.selectedPeriod()
      const loading = this.loading()
      if (mac && !loading) {
        this.loadDataUsage(mac, period)
      }
    })
  }

  private async loadDataUsage(mac: string, period: DataUsagePeriod) {
    try {
      const points = await this.service().getDataUsage(mac, period)
      this.dataPoints.set(points)
    } catch (e) {
      console.error('Failed to load data usage:', e)
      this.dataPoints.set([])
    } finally {
      this.initialLoading.set(false)
    }
  }
}
