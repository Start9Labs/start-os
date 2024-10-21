import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { TuiDialogContext, TuiTextfieldControllerModule } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ActionSuccessGroupComponent } from './action-success-group.component'
import { ActionSuccessSingleComponent } from './action-success-single.component'
import { ActionResponseWithResult } from './types'

@Component({
  standalone: true,
  template: `
    <ng-container tuiTextfieldSize="m" [tuiTextfieldLabelOutside]="true">
      <p *ngIf="data.message">{{ data.message }}</p>
      <app-action-success-single
        *ngIf="single"
        [single]="single"
      ></app-action-success-single>
      <app-action-success-group
        *ngIf="group"
        [group]="group"
      ></app-action-success-group>
    </ng-container>
  `,
  imports: [
    CommonModule,
    ActionSuccessGroupComponent,
    ActionSuccessSingleComponent,
    TuiTextfieldControllerModule,
  ],
})
export class ActionSuccessPage {
  readonly data =
    inject<TuiDialogContext<void, ActionResponseWithResult>>(
      POLYMORPHEUS_CONTEXT,
    ).data

  readonly single = this.data.result.type === 'single' ? this.data.result : null
  readonly group = this.data.result.type === 'group' ? this.data.result : null
}
