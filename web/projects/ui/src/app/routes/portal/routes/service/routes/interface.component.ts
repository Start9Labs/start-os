import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAddresses } from '../../../components/interfaces/interface.utils'

@Component({
  template: `
    <app-interface
      *ngIf="interfaceInfo$ | async as interfaceInfo"
      [packageContext]="context"
      [serviceInterface]="interfaceInfo"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceComponent],
})
export class ServiceInterfaceRoute {
  private readonly route = inject(ActivatedRoute)

  readonly context = {
    packageId: getPkgId(this.route),
    interfaceId: this.route.snapshot.paramMap.get('interfaceId') || '',
  }

  readonly interfaceInfo$ = inject<PatchDB<DataModel>>(PatchDB)
    .watch$(
      'packageData',
      this.context.packageId,
      'serviceInterfaces',
      this.context.interfaceId,
    )
    .pipe(
      map(info => ({
        ...info,
        addresses: getAddresses(info),
      })),
    )
}
