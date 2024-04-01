import { DatePipe } from '@angular/common'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TuiIconModule } from '@taiga-ui/experimental'
import { map, timer } from 'rxjs'
import { MetricsComponent } from './metrics.component'
import { ServicesComponent } from './services.component'
import { UtilitiesComponent } from './utilities.component'

@Component({
  standalone: true,
  template: `
    <time>{{ date() | date: 'medium' }}</time>
    <app-metrics>
      <h2>
        <tui-icon icon="tuiIconActivity" />
        Metrics
      </h2>
      <div class="g-plaque"></div>
    </app-metrics>
    <app-utilities>
      <h2>
        <tui-icon icon="tuiIconSettings" />
        Utilities
      </h2>
      <div class="g-plaque"></div>
    </app-utilities>
    <app-services>
      <h2>
        <tui-icon icon="tuiIconGrid" />
        Services
      </h2>
      <div class="g-plaque"></div>
    </app-services>
  `,
  styles: `
    :host {
      position: relative;
      max-width: 64rem;
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      gap: 1rem;
      margin: 2rem auto;
      border: 0.375rem solid transparent;
    }

    app-metrics,
    app-utilities,
    app-services {
      position: relative;
      clip-path: var(--clip-path);
      backdrop-filter: blur(1rem);
      font-size: 1rem;
    }

    time {
      position: absolute;
      left: 22%;
      font-weight: bold;
      line-height: 1.75rem;
    }

    h2 {
      height: 2rem;
      display: flex;
      align-items: center;
      gap: 0.5rem;
      margin: 0;
      padding: 0 2rem;
      font-weight: bold;
      font-size: 1rem;

      tui-icon {
        font-size: 1rem;
      }
    }

    :host-context(tui-root._mobile) {
      height: 100%;
      display: block;
      margin: 0;
      border-top: 0;

      app-metrics,
      app-utilities,
      app-services {
        display: none;
      }

      time,
      h2 {
        display: none;
      }
    }

    :host-context(tui-root._mobile [data-dashboard='metrics']) {
      app-metrics {
        display: block;
      }
    }

    :host-context(tui-root._mobile [data-dashboard='utilities']) {
      app-utilities {
        display: flex;
        align-items: center;
      }
    }

    :host-context(tui-root._mobile main:not([data-dashboard])) {
      app-services {
        display: block;
        margin: 0;
      }
    }
  `,
  imports: [
    ServicesComponent,
    MetricsComponent,
    UtilitiesComponent,
    TuiIconModule,
    DatePipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DashboardComponent {
  readonly date = toSignal(timer(0, 1000).pipe(map(() => new Date())))
}
