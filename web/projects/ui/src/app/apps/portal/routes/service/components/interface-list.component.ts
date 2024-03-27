import { NgForOf } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageStatus } from 'src/app/services/pkg-status-rendering.service'
import { InterfaceInfoPipe } from '../pipes/interface-info.pipe'
import { ServiceInterfaceListItemComponent } from './interface-list-item.component'
import { RouterLink } from '@angular/router'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'service-interface-list',
  template: `
    <h3 class="g-title">Service Interfaces</h3>
    <a
      *ngFor="let info of pkg | interfaceInfo"
      class="g-action"
      [serviceInterfaceListItem]="info"
      [disabled]="status.primary !== 'running'"
      [routerLink]="info.routerLink"
    ></a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    NgForOf,
    RouterLink,
    InterfaceInfoPipe,
    ServiceInterfaceListItemComponent,
  ],
})
export class ServiceInterfaceListComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  status!: PackageStatus
}
