import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TuiTable } from '@taiga-ui/addon-table'
import { tuiDefaultSort } from '@taiga-ui/cdk'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getAddresses } from '../../../components/interfaces/interface.utils'
import { ServiceInterfaceComponent } from './interface.component'

@Component({
  standalone: true,
  selector: 'service-interfaces',
  template: `
    <header>Interfaces</header>
    <table tuiTable class="g-table">
      <thead>
        <tr>
          <th tuiTh>Name</th>
          <th tuiTh>Type</th>
          <th tuiTh>Description</th>
          <th tuiTh>Hosting</th>
          <th tuiTh></th>
        </tr>
      </thead>
      <tbody>
        @for (info of interfaces(); track $index) {
          <tr
            serviceInterface
            [routerLink]="info.routerLink"
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
      grid-column: span 4;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [ServiceInterfaceComponent, TuiTable, RouterLink],
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

        return {
          ...value,
          public: !!host?.bindings[value.addressInfo.internalPort].net.public,
          addresses: host ? getAddresses(value, host, this.config) : {},
          routerLink: `./interface/${id}`,
        }
      }),
  )
}
