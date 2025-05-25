import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { map, timer } from 'rxjs'
import { distinctUntilChanged } from 'rxjs/operators'

@Component({
  selector: 'service-uptime',
  template: `
    <header>{{ 'Uptime' | i18n }}</header>
    <section>
      @if (uptime$ | async; as time) {
        <div>
          <label>{{ time.days }}</label>
          <span>{{ 'Days' | i18n }}</span>
        </div>
        <div>
          <label>{{ time.hours }}</label>
          <span>{{ 'Hours' | i18n }}</span>
        </div>
        <div>
          <label>{{ time.minutes }}</label>
          <span>{{ 'Minutes' | i18n }}</span>
        </div>
        <div>
          <label>{{ time.seconds }}</label>
          <span>{{ 'Seconds' | i18n }}</span>
        </div>
      }
    </section>
  `,
  styles: [
    `
      :host {
        grid-column: span 4;
      }

      h3 {
        font: var(--tui-font-heading-4);
        font-weight: normal;
        margin: 0;
        text-align: center;
      }

      section {
        height: 100%;
        max-width: 100%;
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 1rem;
        place-content: center;
        margin: auto;
        padding: 1rem 0;
        text-align: center;
        text-transform: uppercase;
        color: var(--tui-text-secondary);
        font: var(--tui-font-text-ui-xs);
      }

      label {
        display: block;
        font-size: min(6vw, 2.5rem);
        margin: 1rem 0;
        color: var(--tui-text-primary);
      }

      :host-context(table) {
        padding: 0;

        header {
          display: none;
        }

        section {
          display: flex;
          justify-content: flex-start;
          gap: 0.5rem;
          padding: 0;
          line-height: 1;
        }

        label {
          margin: 0;
          font-size: 1rem;
        }

        div {
          display: flex;
          align-items: baseline;
        }

        span {
          font-size: 0;

          &:first-letter {
            font-size: 0.75rem;
          }
        }
      }

      :host-context(tui-root._mobile table) {
        section {
          min-height: 1.25rem;
          align-items: flex-end;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [i18nPipe, AsyncPipe],
})
export class ServiceUptimeComponent {
  protected readonly uptime$ = timer(0, 1000).pipe(
    map(() =>
      this.started()
        ? Math.max(Date.now() - new Date(this.started()).getTime(), 0)
        : 0,
    ),
    distinctUntilChanged(),
    map(delta => ({
      seconds: Math.floor(delta / 1000) % 60,
      minutes: Math.floor(delta / (1000 * 60)) % 60,
      hours: Math.floor(delta / (1000 * 60 * 60)) % 24,
      days: Math.floor(delta / (1000 * 60 * 60 * 24)),
    })),
  )

  readonly started = input('')
}
