import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { tuiDefaultSort } from '@taiga-ui/cdk'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ServiceInterfaceItemComponent } from './interface-item.component'
import { i18nPipe } from '@start9labs/shared'
import { PlaceholderComponent } from '../../../components/placeholder.component'

@Component({
  selector: 'service-interfaces',
  template: `
    <header>{{ 'Service Interfaces' | i18n }}</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>{{ 'Name' | i18n }}</th>
          <th tuiTh>{{ 'Type' | i18n }}</th>
          <th tuiTh>{{ 'Description' | i18n }}</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (info of interfaces(); track $index) {
          <tr
            tabindex="-1"
            serviceInterface
            [info]="info"
            [pkg]="pkg()"
            [disabled]="disabled()"
            [routerLink]="info.routerLink"
          >
            <a [routerLink]="info.routerLink">
              <strong>{{ info.name }}</strong>
            </a>
          </tr>
        } @empty {
          <app-placeholder icon="@tui.monitor-x">
            {{ 'No service interfaces' | i18n }}
          </app-placeholder>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      grid-column: span 7;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    ServiceInterfaceItemComponent,
    TuiTable,
    i18nPipe,
    PlaceholderComponent,
    RouterLink,
  ],
})
export class ServiceInterfacesComponent {
  readonly pkg = input.required<PackageDataEntry>()
  readonly disabled = input(false)

  readonly interfaces = computed(({ serviceInterfaces } = this.pkg()) =>
    Object.entries(serviceInterfaces)
      .sort((a, b) => tuiDefaultSort(a[1], b[1]))
      .map(([id, value]) => {
        return {
          ...value,
          routerLink: `./interface/${id}`,
        }
      }),
  )
}
