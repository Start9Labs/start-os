import { NgForOf } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackagePlus } from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryStatus,
} from 'src/app/services/pkg-status-rendering.service'
import { InterfaceInfoPipe } from '../pipes/interface-info.pipe'
import { ServiceInterfaceComponent } from './interface.component'
import { RouterLink } from '@angular/router'

@Component({
  selector: 'service-interfaces',
  template: `
    <h3 class="g-title">Interfaces</h3>
    <a
      *ngFor="let info of service.pkg | interfaceInfo"
      class="g-action"
      [serviceInterface]="info"
      [disabled]="!isRunning(service.status)"
      [routerLink]="info.routerLink"
    ></a>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [NgForOf, RouterLink, InterfaceInfoPipe, ServiceInterfaceComponent],
})
export class ServiceInterfacesComponent {
  @Input({ required: true })
  service!: PackagePlus

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }
}
