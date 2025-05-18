import { AsyncPipe } from '@angular/common'
import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { filter, map, startWith, timer } from 'rxjs'

@Component({
  selector: 'service-uptime',
  template: `
    <header>{{ 'Uptime' | i18n }}</header>
    <section>
      @if (uptime$ | async; as time) {
        <div>
          <label>{{ time.days }}</label>
          {{ 'Days' | i18n }}
        </div>
        <div>
          <label>{{ time.hours }}</label>
          {{ 'Hours' | i18n }}
        </div>
        <div>
          <label>{{ time.minutes }}</label>
          {{ 'Minutes' | i18n }}
        </div>
        <div>
          <label>{{ time.seconds }}</label>
          {{ 'Seconds' | i18n }}
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
        width: fit-content;
        display: grid;
        grid-template-columns: 1fr 1fr 1fr 1fr;
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
        font-size: 2.5rem;
        margin: 1rem;
        color: var(--tui-text-primary);
      }
    `,
  ],
  host: { class: 'g-card' },
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [i18nPipe, AsyncPipe],
})
export class ServiceUptimeComponent {
  protected readonly uptime$ = timer(0, 1000).pipe(
    filter(() => !!this.started()),
    map(() => Date.now() - new Date(this.started()).getTime()),
    startWith(0),
    map(delta => ({
      seconds: Math.floor(delta / 1000),
      minutes: Math.floor((delta / (1000 * 60)) % 60),
      hours: Math.floor((delta / (1000 * 60 * 60)) % 24),
      days: Math.floor(delta / (1000 * 60 * 60 * 24)),
    })),
  )

  readonly started = input('')
}
