import { CommonModule } from '@angular/common'
import { Component, inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ActionSuccessGroupComponent } from './action-success-group.component'
import { ActionSuccessSingleComponent } from './action-success-single.component'
import { ActionResponseWithResult } from './types'

@Component({
  standalone: true,
  template: `
    <p *ngIf="data.message">{{ data.message }}</p>
    <app-action-success-single
      *ngIf="single"
      [single]="single"
    ></app-action-success-single>
    <app-action-success-group
      *ngIf="group"
      [group]="group"
    ></app-action-success-group>
  `,
  imports: [
    CommonModule,
    ActionSuccessGroupComponent,
    ActionSuccessSingleComponent,
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
