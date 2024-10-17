import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { TuiDialogContext, TuiTextfieldControllerModule } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { RR } from 'src/app/services/api/api.types'
import { ActionSuccessGroupComponent } from './action-success-group.component'
import { ActionSuccessItemComponent } from './action-success-item.component'

@Component({
  standalone: true,
  template: `
    <ng-container tuiTextfieldSize="m" [tuiTextfieldLabelOutside]="true">
      <app-action-success-item
        *ngIf="item"
        [value]="item"
      ></app-action-success-item>
      <app-action-success-group
        *ngIf="group"
        [value]="group"
      ></app-action-success-group>
    </ng-container>
  `,
  imports: [
    CommonModule,
    ActionSuccessGroupComponent,
    ActionSuccessItemComponent,
    TuiTextfieldControllerModule,
  ],
})
export class ActionSuccessPage {
  readonly data =
    inject<TuiDialogContext<void, RR.ActionRes>>(POLYMORPHEUS_CONTEXT).data

  readonly item = this.data?.type === 'string' ? this.data : null
  readonly group = this.data?.type === 'object' ? this.data : null
}
