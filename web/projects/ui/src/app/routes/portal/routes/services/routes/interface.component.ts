import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAddresses } from '../../../components/interfaces/interface.utils'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  template: `
    <app-interface
      *ngIf="interfacesWithAddresses$ | async as serviceInterface"
      [packageContext]="context"
      [serviceInterface]="serviceInterface"
    />
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceComponent],
})
export default class ServiceInterfaceRoute {
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly config = inject(ConfigService)

  readonly context = {
    packageId: getPkgId(),
    interfaceId:
      inject(ActivatedRoute).snapshot.paramMap.get('interfaceId') || '',
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
      addresses: getAddresses(
        iFace,
        hosts[iFace.addressInfo.hostId],
        this.config,
      ),
    })),
  )
}
