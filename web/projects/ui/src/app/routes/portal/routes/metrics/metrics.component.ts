import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { TitleDirective } from 'src/app/services/title.service'
import { CpuComponent } from './cpu.component'
import { MemoryComponent } from './memory.component'
import { MetricsService } from './metrics.service'
import { StorageComponent } from './storage.component'
import { TemperatureComponent } from './temperature.component'
import { TimeComponent } from './time.component'
import { UptimeComponent } from './uptime.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'app-metrics',
  template: `
    <ng-container *title>{{ 'Metrics' | i18n }}</ng-container>
    <div>
      <section class="g-card">
        <header>{{ 'System Time' | i18n }}</header>
        <metrics-time />
      </section>
      <section class="g-card">
        <header>{{ 'Uptime' | i18n }}</header>
        <metrics-uptime />
      </section>
      <section class="g-card">
        <header>{{ 'Temperature' | i18n }}</header>
        <metrics-temperature [value]="temperature()" />
      </section>
      <section class="g-card">
        <header>CPU</header>
        <metrics-cpu [value]="metrics()?.cpu" />
      </section>
      <section class="g-card">
        <header>{{ 'Memory' | i18n }}</header>
        <metrics-memory [value]="metrics()?.memory" />
      </section>
      <section class="g-card">
        <header>{{ 'Storage' | i18n }}</header>
        <metrics-storage [value]="metrics()?.disk" />
      </section>
    </div>
  `,
  styles: `
    :host {
      padding: 1rem;

      div {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        grid-auto-flow: dense;
        gap: 1rem;
      }

      header {
        background: transparent;
      }
    }

    :host-context(tui-root._mobile) {
      .g-card {
        grid-column: span 3;

        &:nth-child(1),
        &:nth-child(2) {
          grid-column: span 2;
        }

        &:nth-child(3) {
          grid-column: span 1;
          grid-row: span 2;
          padding-top: 0;

          header {
            display: none;
          }
        }
      }
    }
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TemperatureComponent,
    StorageComponent,
    CpuComponent,
    TitleDirective,
    MemoryComponent,
    UptimeComponent,
    TimeComponent,
    i18nPipe,
  ],
})
export default class SystemMetricsComponent {
  readonly metrics = toSignal(inject(MetricsService))

  readonly temperature = computed(() =>
    Number(this.metrics()?.general.temperature?.value || 0),
  )
}
