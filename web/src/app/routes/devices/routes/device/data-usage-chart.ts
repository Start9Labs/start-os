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
import {
  TuiButton,
  TuiDataList,
  TuiDropdown,
  TuiLoader,
  TuiNotification,
} from '@taiga-ui/core'
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
      <div class="chart-message">
        <tui-loader />
      </div>
    } @else if (error()) {
      <div tuiNotification appearance="negative" class="chart-error">
        Failed to load data usage.
        <button tuiButton size="s" appearance="secondary" (click)="retry()">
          Retry
        </button>
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
        @if (isEmpty()) {
          <div class="chart-empty">No data usage recorded for this period.</div>
        } @else {
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

    .chart-message {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 10rem;
    }

    .chart-empty {
      position: absolute;
      inset: 0;
      display: flex;
      align-items: center;
      justify-content: center;
      color: var(--tui-text-secondary);
      font-size: 0.875rem;
    }

    .chart-error {
      display: flex;
      align-items: center;
      gap: 0.75rem;
      margin: 0.5rem 0;
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
    TuiNotification,
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
  protected readonly periods: DataUsagePeriod[] = ['week', 'month', '3months']

  protected readonly selectedPeriod = signal<DataUsagePeriod>('week')
  protected readonly initialLoading = signal(true)
  protected readonly dataPoints = signal<DataUsagePoint[]>([])
  protected readonly error = signal(false)

  // Backend zero-fills missing days, so an "all zero" series is the real
  // empty state — render the message instead of flat-zero lines.
  protected readonly isEmpty = computed(() => {
    const points = this.dataPoints()
    return (
      points.length === 0 ||
      points.every(p => p.download === 0 && p.upload === 0)
    )
  })

  // Convert data to line chart format using absolute coordinates.
  // A single point is drawn as a flat segment.
  protected readonly chartLines = computed((): TuiPoint[][] => {
    const points = this.dataPoints()
    if (points.length === 0) return []

    const width = 400
    const height = 160
    const maxBytes = Math.max(
      ...points.map(p => Math.max(p.download, p.upload)),
    )
    const scale = maxBytes > 0 ? height / maxBytes : 1

    if (points.length === 1) {
      const [p] = points
      return [
        [
          [0, p.download * scale],
          [width, p.download * scale],
        ],
        [
          [0, p.upload * scale],
          [width, p.upload * scale],
        ],
      ]
    }

    const len = points.length - 1
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
      case 'week': {
        // 7 daily points ending today — rotate so the last label is today.
        const days = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
        const today = new Date().getDay()
        return Array.from({ length: 7 }, (_, i) => days[(today + 1 + i) % 7])
      }
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

  protected retry() {
    const mac = this.mac()
    if (mac) {
      this.loadDataUsage(mac, this.selectedPeriod())
    }
  }

  private async loadDataUsage(mac: string, period: DataUsagePeriod) {
    this.error.set(false)
    try {
      const points = await this.service().getDataUsage(mac, period)
      this.dataPoints.set(points)
    } catch (e) {
      console.error('Failed to load data usage:', e)
      this.dataPoints.set([])
      this.error.set(true)
    } finally {
      this.initialLoading.set(false)
    }
  }
}
