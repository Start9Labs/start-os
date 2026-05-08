import {
  ChangeDetectionStrategy,
  Component,
  inject,
  linkedSignal,
  model,
  WritableSignal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiAutoColorPipe, TuiTimeline } from '@taiga-ui/kit'
import { ScheduleWindow } from 'src/app/services/api/api.service'
import {
  formatTime12h,
  quarterHourToTime,
  timeToQuarterHour,
} from 'src/app/utils/schedule'
import { ADD_SCHEDULE_WINDOW } from './dialog'

type ViewWindow = {
  range: readonly [number, number]
  days: ScheduleWindow['days']
}

@Component({
  selector: 'profile-schedule-editor',
  template: `
    <section (change)="commit()">
      @for (day of order; track $index) {
        <tui-timeline
          #timeline
          orientation="vertical"
          [max]="96"
          [template]="gap"
        >
          <span class="day">{{ labels[day] }}</span>
          @for (window of view(); track $index) {
            @if (window.days[day]) {
              <label
                tuiTimelineItem
                [style.--color]="(($index + 1) * 123).toString() | tuiAutoColor"
                [(value)]="window.range"
                (dblclick)="edit($index)"
              >
                <tui-icon icon="@tui.ellipsis" />
                <span class="time" [innerHTML]="getTime(window.range)"></span>
                <tui-icon icon="@tui.ellipsis" />
              </label>
            }
          }
          <ng-template #gap let-index>
            <div [style.width.%]="100" [style.overflow]="'hidden'">
              <button
                tuiIconButton
                iconStart="@tui.plus"
                size="s"
                appearance="secondary-grayscale"
                (click)="add(day, index, timeline.value())"
              >
                Add
              </button>
            </div>
          </ng-template>
        </tui-timeline>
      }
    </section>
  `,
  styles: `
    section {
      display: grid;
      grid-auto-flow: column;
      height: 22rem;
      gap: 0.125rem;
      border-radius: var(--tui-radius-s);
      overflow: hidden;
      margin-top: 1.5rem;
    }

    tui-timeline {
      background: var(--tui-background-neutral-1);
      border-radius: var(--tui-radius-xs);
      border-inline-start: 2rem solid transparent;
    }

    [tuiTimelineItem] {
      background: var(--tui-background-neutral-1);
      box-shadow: inset 0 0 0 0.875rem var(--tui-background-neutral-1-hover);
      clip-path: inset(1px 2px round 0.125rem);

      &::before {
        content: '';
        position: absolute;
        inset: 0;
        background: var(--color);
        opacity: 0.25;
      }
    }

    .day {
      writing-mode: horizontal-tb;
      position: absolute;
      top: -2rem;
      width: 100%;
      text-align: center;
      line-height: calc(2rem - 1px);
      border-radius: inherit;
      border-bottom-left-radius: 0;
      border-bottom-right-radius: 0;
      background: var(--tui-border-normal);
    }

    tui-icon {
      position: absolute;
      width: 100%;

      &:first-of-type {
        top: -0.25rem;
      }

      &:last-of-type {
        bottom: -0.25rem;
      }
    }

    .time {
      position: absolute;
      writing-mode: horizontal-tb;
      display: flex;
      align-items: center;
      inset: 0;
      justify-content: center;
      text-align: center;
      line-height: 1;
      clip-path: inset(1rem);
    }

    button {
      inset: 50%;
      border-radius: 100%;
      transform: translate(-50%, -50%);
      visibility: hidden;
    }

    tui-timeline:hover button {
      visibility: visible;
    }

    :host-context(tui-root._mobile) {
      section {
        width: fit-content;
        grid-auto-columns: max-content;
      }

      tui-timeline {
        width: 1.75rem;
      }

      section:hover {
        tui-timeline:hover {
          width: 6rem;
        }

        tui-timeline:not(:hover) .day {
          font-size: 0;
        }
      }

      section:not(:hover) {
        tui-timeline:first-child {
          width: 6rem;
        }

        tui-timeline:not(:first-child) {
          .day {
            font-size: 0;
          }
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTimeline, TuiIcon, TuiAutoColorPipe, TuiButton],
})
export class ProfileScheduleEditor {
  protected readonly order = [1, 2, 3, 4, 5, 6, 0] as const
  protected readonly labels = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
  protected readonly dialogs = inject(TuiResponsiveDialogService)

  readonly windows = model<readonly ScheduleWindow[]>([])

  protected readonly view: WritableSignal<ViewWindow[]> = linkedSignal({
    source: () =>
      this.windows().map(({ startTime, endTime, days }) => ({
        range: [from(startTime), from(endTime)] as const,
        days,
      })),
    computation: source => source,
  })

  protected getTime(range: readonly [number, number]): string {
    return `${format(to(range[0]))}<br />-<br />${format(to(range[1]))}`
  }

  protected edit(index: number) {
    this.dialogs
      .open<ScheduleWindow | null>(ADD_SCHEDULE_WINDOW, {
        label: 'Edit WAN-Restricted Window',
        data: {
          startTime: to(this.view()[index].range[0]),
          endTime: to(this.view()[index].range[1]),
          days: this.view()[index].days,
        },
      })
      .subscribe(value => {
        this.view.update(view =>
          view
            .map<any>((item, i) =>
              i === index
                ? value && {
                    range: [from(value.startTime), from(value.endTime)],
                    days: value.days,
                  }
                : item,
            )
            .filter(Boolean),
        )
        this.commit()
      })
  }

  protected add(
    day: number,
    index: number,
    ranges: (readonly [number, number])[],
  ) {
    const days = this.order.map(() => false)
    const start = ranges.map(([_, end]) => end).sort()[index - 1] || 0

    days[day] = true
    this.dialogs
      .open<ScheduleWindow>(ADD_SCHEDULE_WINDOW, {
        label: 'Add WAN-Restricted Window',
        data: {
          startTime: to(start),
          endTime: to(start + 4),
          days,
        },
      })
      .subscribe(({ days, startTime, endTime }) => {
        this.view.update(view =>
          view.concat({ range: [from(startTime), from(endTime)], days }),
        )
        this.commit()
      })
  }

  protected commit() {
    this.windows.set(
      this.view().map(({ range, days }) => ({
        startTime: to(range[0]),
        endTime: to(range[1]),
        days,
      })),
    )
  }
}

const to = quarterHourToTime
const from = timeToQuarterHour
const format = formatTime12h
