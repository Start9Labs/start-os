import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { InterfacesComponent } from 'src/app/apps/portal/components/interfaces/interfaces.component'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <app-interfaces
      *ngIf="ui$ | async as ui"
      [style.max-width.rem]="50"
      [addressInfo]="ui"
      [isUi]="true"
    />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, InterfacesComponent],
})
export class SettingsInterfacesComponent {
  readonly ui$ = inject(PatchDB<DataModel>).watch$('server-info', 'ui')
}
