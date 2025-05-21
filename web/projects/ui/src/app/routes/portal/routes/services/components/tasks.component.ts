import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { TuiTable } from '@taiga-ui/addon-table'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServiceTaskComponent } from './task.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'service-tasks',
  template: `
    <header>{{ 'Tasks' | i18n }}</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>{{ 'Service' | i18n }}</th>
          <th tuiTh>{{ 'Action' }}</th>
          <th tuiTh>{{ 'Severity' }}</th>
          <th tuiTh>{{ 'Description' | i18n }}</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (item of tasks(); track $index) {
          <tr [task]="item.task" [services]="services()"></tr>
        }
      </tbody>
    </table>
    @if (!tasks().length) {
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
  imports: [TuiTable, ServiceTaskComponent, PlaceholderComponent, i18nPipe],
})
export class ServiceTasksComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly tasks = computed(() =>
    Object.values(this.pkg().tasks)
      .filter(
        t =>
          this.services()[t.task.packageId]?.actions[t.task.actionId] &&
          t.active,
      )
      .sort((a, b) => a.task.severity.localeCompare(b.task.severity)),
  )
}
