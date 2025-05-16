import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { TuiTable } from '@taiga-ui/addon-table'
import { tuiDefaultSort } from '@taiga-ui/cdk'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getAddresses } from '../../../components/interfaces/interface.utils'
import { ServiceInterfaceComponent } from './interface.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'service-interfaces',
  template: `
    <header>{{ 'Interfaces' | i18n }}</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>{{ 'Name' | i18n }}</th>
          <th tuiTh>{{ 'Type' | i18n }}</th>
          <th tuiTh [style.text-align]="'center'">{{ 'Hosting' | i18n }}</th>
          <th tuiTh>{{ 'Description' | i18n }}</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (info of interfaces(); track $index) {
          <tr
            serviceInterface
            [info]="info"
            [pkg]="pkg()"
            [disabled]="disabled()"
          ></tr>
        }
      </tbody>
    </table>
  `,
  styles: `
    :host {
      grid-column: span 6;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceInterfaceComponent, TuiTable, i18nPipe],
})
export class ServiceInterfacesComponent {
  private readonly config = inject(ConfigService)

  readonly pkg = input.required<PackageDataEntry>()
  readonly disabled = input(false)

  readonly interfaces = computed(({ serviceInterfaces, hosts } = this.pkg()) =>
    Object.entries(serviceInterfaces)
      .sort((a, b) => tuiDefaultSort(a[1], b[1]))
      .map(([id, value]) => {
        const host = hosts[value.addressInfo.hostId]
        const port = value.addressInfo.internalPort

        return {
          ...value,
          addSsl: host?.bindings[port]?.options.addSsl,
          public: !!host?.bindings[port]?.net.public,
          addresses: host ? getAddresses(value, host, this.config) : {},
          routerLink: `./interface/${id}`,
        }
      }),
  )
}
