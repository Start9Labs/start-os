import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { InterfaceAddressesComponentModule } from 'src/app/common/interface-addresses/interface-addresses.module'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <interface-addresses
      *ngIf="ui$ | async as ui"
      [style.max-width.rem]="50"
      [addressInfo]="ui"
      [isUi]="true"
    ></interface-addresses>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfaceAddressesComponentModule],
})
export class SettingsInterfacesComponent {
  readonly ui$ = inject(PatchDB<DataModel>).watch$('server-info', 'ui')
}
