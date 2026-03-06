import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  linkedSignal,
  WritableSignal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiAutoColorPipe, TuiTimeline } from '@taiga-ui/kit'
import { ADD_BLACKOUT_WINDOW } from 'src/app/routes/wifi/routes/blackout-schedule/dialog'
import { BlackoutService, BlackoutWindow } from './service'

@Component({
  template: `
    <section (change)="save()">
      @for (day of order; track $index) {
        <tui-timeline
          #timeline
          orientation="vertical"
          [max]="96"
          [template]="gap"
        >
          <span class="day">{{ labels[day] }}</span>
          @for (window of windows(); track $index) {
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
    :host {
      max-width: 50rem;
      margin-bottom: -5rem;

      &::after {
        display: none;
      }
    }

    section {
      display: grid;
      grid-auto-flow: column;
      height: 36rem;
      max-height: calc(100svh - 12rem);
      gap: 0.125rem;
      border-radius: var(--tui-radius-s);
      overflow: hidden;
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
  host: { class: 'g-page' },
  imports: [TuiTimeline, TuiIcon, TuiAutoColorPipe, TuiButton],
})
export default class BlackoutScheduleComponent {
  // Display order: Mon–Sun; data order: Sun(0)–Sat(6)
  protected readonly order = [1, 2, 3, 4, 5, 6, 0] as const
  protected readonly labels = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
  protected readonly dialogs = inject(TuiResponsiveDialogService)
  protected readonly service = inject(BlackoutService)
  protected readonly source = computed(
    () =>
      this.service.data()?.map(({ startTime, endTime, days }) => ({
        range: [from(startTime), from(endTime)] as const,
        days,
      })) || [],
  )

  protected readonly windows: WritableSignal<ReturnType<typeof this.source>> =
    linkedSignal({
      source: this.source,
      computation: (source, current) =>
        current?.value.length ? current.value : source,
    })

  protected getTime(range: readonly [number, number]): string {
    return `${format(to(range[0]))}<br />-<br />${format(to(range[1]))}`
  }

  protected edit(index: number) {
    this.dialogs
      .open<BlackoutWindow | null>(ADD_BLACKOUT_WINDOW, {
        label: 'Edit Blackout Window',
        data: {
          startTime: to(this.windows()[index].range[0]),
          endTime: to(this.windows()[index].range[1]),
          days: this.windows()[index].days,
        },
      })
      .subscribe(value => {
        this.windows.update(windows =>
          windows
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
        this.save()
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
      .open<BlackoutWindow>(ADD_BLACKOUT_WINDOW, {
        label: 'Add Blackout Window',
        data: {
          startTime: to(start),
          endTime: to(start + 4),
          days,
        },
      })
      .subscribe(({ days, startTime, endTime }) => {
        this.windows.update(windows =>
          windows.concat({ range: [from(startTime), from(endTime)], days }),
        )
        this.save()
      })
  }

  protected save() {
    this.service.store(
      this.windows().map(({ range, days }) => ({
        startTime: to(range[0]),
        endTime: to(range[1]),
        days,
      })),
    )
  }
}

function to(quarterHours: number): string {
  return `${Math.floor(quarterHours / 4)
    .toString()
    .padStart(2, '0')}:${(quarterHours % 4) * 15}`
}

function from(formatted: string): number {
  const [h, m] = formatted.split(':').map(Number)

  return h * 4 + Math.round(m / 15)
}

function format(time: string): string {
  const [h, m] = time.split(':').map(Number)
  const period = h >= 12 ? 'pm' : 'am'
  const hour = h % 12 || 12

  return h === 24
    ? `11:59pm`
    : `${hour}:${m.toString().padStart(2, '0')}${period}`
}
