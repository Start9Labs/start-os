import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getMultihostAddresses } from '../../../components/interfaces/interface.utils'

@Component({
  template: `
    <app-interface
      *ngIf="interfacesWithAddresses$ | async as serviceInterface"
      [packageContext]="context"
      [serviceInterface]="serviceInterface"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceComponent],
})
export class ServiceInterfaceRoute {
  private readonly route = inject(ActivatedRoute)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly context = {
    packageId: getPkgId(this.route),
    interfaceId: this.route.snapshot.paramMap.get('interfaceId') || '',
  }

  readonly interfacesWithAddresses$ = combineLatest([
    this.patch.watch$(
      'packageData',
      this.context.packageId,
      'serviceInterfaces',
      this.context.interfaceId,
    ),
    this.patch.watch$('packageData', this.context.packageId, 'hosts'),
  ]).pipe(
    map(([iFace, hosts]) => ({
      ...iFace,
      addresses: getMultihostAddresses(iFace, hosts[iFace.addressInfo.hostId]),
    })),
  )
}
