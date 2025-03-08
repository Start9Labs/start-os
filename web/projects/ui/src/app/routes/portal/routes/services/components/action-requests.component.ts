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

@Component({
  standalone: true,
  selector: 'service-action-requests',
  template: `
    <header>Tasks</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>Service</th>
          <th tuiTh>Type</th>
          <th tuiTh>Description</th>
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
        All tasks complete
      </app-placeholder>
    }
  `,
  styles: `
    :host {
      grid-column: span 6;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiTable, ServiceActionRequestComponent, PlaceholderComponent],
})
export class ServiceActionRequestsComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly requests = computed(() =>
    Object.values(this.pkg().requestedActions)
      .filter(r => r.active)
      .sort((a, b) => a.request.severity.localeCompare(b.request.severity)),
  )
}
