import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TUI_ALWAYS_DASHED, TuiAxes, TuiBarChart } from '@taiga-ui/addon-charts'
import { TuiButton, TuiHint, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiChevron } from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Summary } from 'src/app/components/summary'

import DevicesDevice from '.'

@Component({
  selector: '[deviceSummary]',
  template: `
    @if (parent.form.valueChanges | async) {}
    <header tuiHeader>
      <h2 tuiTitle>Summary</h2>
      <aside tuiAccessories>
        <button tuiButton appearance="primary-destructive">Block</button>
      </aside>
    </header>
    <section>
      <div appSummary>
        Name
        <span tuiSubtitle>{{ parent.form.value.name }}</span>
      </div>
      <div appSummary>
        Permissions
        <span tuiSubtitle>
          <tui-icon icon="@tui.scroll" />
          Admin
        </span>
      </div>
      <div appSummary>
        Connection
        <span tuiSubtitle>
          <tui-icon icon="@tui.wifi" />
          Child
        </span>
      </div>
      <div appSummary>
        Speed
        <span tuiSubtitle>
          <tui-icon icon="@tui.arrow-up" />
          1.21
          <small>MB/S</small>
        </span>
        <span tuiSubtitle>
          <tui-icon icon="@tui.arrow-down" />
          237
          <small>MB/S</small>
        </span>
      </div>
      <div appSummary>
        <header>
          Data Usage
          <button tuiButton tuiChevron appearance="secondary-grayscale">
            Weekly
          </button>
        </header>
        <tui-axes
          axisY="none"
          [axisXLabels]="x"
          [axisYSecondaryLabels]="y"
          [horizontalLines]="3"
          [horizontalLinesHandler]="lines"
        >
          <tui-bar-chart
            size="l"
            [max]="3"
            [tuiHintContent]="hint"
            [value]="value"
          />
          <ng-template #hint let-index>
            {{ value[0][index] }}
            <small>GB</small>
          </ng-template>
        </tui-axes>
      </div>
    </section>
  `,
  styles: `
    section {
      display: grid;
      grid-template-columns: 8rem 1fr;
    }

    tui-icon {
      font-size: 1rem;
    }

    tui-axes {
      height: 100%;

      ::ng-deep ~ * {
        display: none !important;
      }
    }

    [appSummary] {
      grid-column: 1;
      flex: 1 1 auto;

      &:last-child {
        grid-area: 1 / 2 / 5 / 2;
        min-height: 12rem;
      }

      header {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        padding-block-end: 1rem;
      }
    }

    [tuiSubtitle] {
      gap: 0.25rem;
    }

    :host-context(tui-root._mobile) {
      section {
        display: flex;
      }
    }
  `,
  host: { '[style.background]': '"var(--tui-status-info-pale)"' },
  imports: [
    AsyncPipe,
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiIcon,
    TuiAxes,
    TuiChevron,
    TuiBarChart,
    TuiHint,
    Summary,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DeviceSummary {
  protected readonly parent = inject(DevicesDevice)
  protected readonly x = ['MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT', 'SUN']
  protected readonly y = ['0', '1GB', '2GB', '3GB']
  protected readonly lines = TUI_ALWAYS_DASHED
  protected readonly value = [[1, 2.5, 1.2, 2, 0.5, 2.8, 1.5]]
}
