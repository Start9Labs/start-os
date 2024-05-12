import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PackageStatus } from 'src/app/services/pkg-status-rendering.service'
import { InterfaceInfoPipe } from '../pipes/interface-info.pipe'
import { ServiceInterfaceListItemComponent } from './interface-list-item.component'

@Component({
  selector: 'service-interface-list',
  template: `
    @for (info of pkg | interfaceInfo; track $index) {
      <a
        class="g-action"
        [serviceInterfaceListItem]="info"
        [disabled]="status.primary !== 'running'"
        [routerLink]="info.routerLink"
      ></a>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [RouterLink, InterfaceInfoPipe, ServiceInterfaceListItemComponent],
})
export class ServiceInterfaceListComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  status!: PackageStatus
}
