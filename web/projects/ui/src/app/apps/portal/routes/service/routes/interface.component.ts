import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { InterfacesComponent } from 'src/app/apps/portal/components/interfaces/interfaces.component'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <app-interfaces
      *ngIf="interfaceInfo$ | async as interfaceInfo"
      [packageContext]="context"
      [addressInfo]="interfaceInfo.addressInfo"
      [isUi]="interfaceInfo.type === 'ui'"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfacesComponent],
})
export class ServiceInterfaceRoute {
  private readonly route = inject(ActivatedRoute)

  readonly context = {
    packageId: getPkgId(this.route),
    interfaceId: this.route.snapshot.paramMap.get('interfaceId') || '',
  }

  readonly interfaceInfo$ = inject(PatchDB<DataModel>).watch$(
    'package-data',
    this.context.packageId,
    'installed',
    'interfaceInfo',
    this.context.interfaceId,
  )
}
