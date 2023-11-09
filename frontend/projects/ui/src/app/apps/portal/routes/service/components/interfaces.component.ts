import { NgForOf } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { InterfaceInfoPipe } from '../pipes/interface-info.pipe'
import { ToStatusPipe } from '../pipes/to-status.pipe'
import { ServiceInterfaceComponent } from './interface.component'
import { RouterLink } from '@angular/router'

@Component({
  selector: 'service-interfaces',
  template: `
    <h3 class="g-title">Interfaces</h3>
    <a
      *ngFor="let info of service | interfaceInfo"
      class="g-action"
      [serviceInterface]="info"
      [disabled]="!isRunning(service | toStatus)"
      [routerLink]="info.routerLink"
    ></a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    NgForOf,
    RouterLink,
    InterfaceInfoPipe,
    ServiceInterfaceComponent,
    ToStatusPipe,
  ],
})
export class ServiceInterfacesComponent {
  @Input({ required: true })
  service!: PackageDataEntry

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }
}
