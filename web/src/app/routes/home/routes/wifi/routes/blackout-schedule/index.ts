import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  linkedSignal,
} from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'
import { TuiAutoColorPipe } from '@taiga-ui/kit'
import { TuiTimeline } from 'src/app/components/timeline'
import { Help } from 'src/app/directives/help'
import { WifiBlackoutAside } from './aside'
import { BlackoutService } from './service'

@Component({
  template: `
    <wifi-blackout-aside *help />
    <section>
      @for (day of order; track $index) {
        <tui-timeline orientation="vertical" [total]="96">
          <label>{{ labels[day] }}</label>
          @for (window of windows(); track $index) {
            @if (window.days[day]) {
              <tui-timeline-item
                [style.--color]="($index * 12345).toString() | tuiAutoColor"
                [(value)]="window.range"
              >
                <tui-icon icon="@tui.ellipsis" />
                <span [innerHTML]="getTime(window.range)"></span>
                <tui-icon icon="@tui.ellipsis" />
              </tui-timeline-item>
            }
          }
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

    tui-timeline-item {
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

    label {
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

    span {
      position: absolute;
      writing-mode: horizontal-tb;
      display: flex;
      align-items: center;
      inset: 0;
      justify-content: center;
      text-align: center;
      line-height: 1;
      clip-path: inset(1rem);

      @media screen and (max-width: 620px) {
        opacity: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  host: { class: 'g-page' },
  imports: [WifiBlackoutAside, Help, TuiTimeline, TuiIcon, TuiAutoColorPipe],
})
export default class BlackoutScheduleComponent {
  // Display order: Mon–Sun; data order: Sun(0)–Sat(6)
  protected readonly order = [1, 2, 3, 4, 5, 6, 0]
  protected readonly labels = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']

  protected getTime(range: readonly [number, number]): string {
    return `${format(to(range[0]))}<br />-<br />${format(to(range[1]))}`
  }

  protected readonly service = inject(BlackoutService)
  protected readonly source = computed(
    () =>
      this.service.data()?.map(({ startTime, endTime, days }) => ({
        range: [from(startTime), from(endTime)] as const,
        days,
      })) || [],
  )

  protected readonly windows = linkedSignal({
    source: this.source,
    computation: (source, current: any) =>
      current?.value.length ? current.value : source,
  })

  delete(index: number) {
    this.service.deleteWindow(index)
  }
}

function to(quarterHours: number): string {
  return `${Math.floor(quarterHours / 4)}:${(quarterHours % 4) * 15}`
}

function from(formatted: string): number {
  const [h, m] = formatted.split(':').map(Number)

  return h * 4 + Math.round(m / 4)
}

function format(time: string): string {
  const [h, m] = time.split(':').map(Number)
  const period = h >= 12 ? 'pm' : 'am'
  const hour = h % 12 || 12

  return h === 24
    ? `11:59pm`
    : `${hour}:${m.toString().padStart(2, '0')}${period}`
}
