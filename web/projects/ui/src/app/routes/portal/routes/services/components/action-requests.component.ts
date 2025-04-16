import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { TuiTable } from '@taiga-ui/addon-table'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServiceActionRequestComponent } from './action-request.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'service-action-requests',
  template: `
    <header>{{ 'Tasks' | i18n }}</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>{{ 'Service' | i18n }}</th>
          <th tuiTh>{{ 'Type' | i18n }}</th>
          <th tuiTh>{{ 'Description' | i18n }}</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of requests(); track $index) {
          <tr [actionRequest]="item.request" [services]="services()"></tr>
        }
      </tbody>
    </table>
    @if (!requests().length) {
      <app-placeholder icon="@tui.list-checks">
        {{ 'All tasks complete' | i18n }}
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      min-height: 12rem;
      grid-column: span 6;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiTable,
    ServiceActionRequestComponent,
    PlaceholderComponent,
    i18nPipe,
  ],
})
export class ServiceActionRequestsComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly requests = computed(() =>
    Object.values(this.pkg().requestedActions)
      // @TODO Alex uncomment filter line below to produce infinite loop on service details page when dependency not installed. This means the page is infinitely trying to re-render
      // .filter(r => r.active)
      .filter(
        r =>
          this.services()[r.request.packageId]?.actions[r.request.actionId] &&
          r.active,
      )
      .sort((a, b) => a.request.severity.localeCompare(b.request.severity)),
  )
}
