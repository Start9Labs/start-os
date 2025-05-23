import { Component, inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@taiga-ui/polymorpheus'
import { ActionSuccessGroupComponent } from './action-success-group.component'
import { ActionSuccessSingleComponent } from './action-success-single.component'
import { ActionResponseWithResult } from './types'

@Component({
  standalone: true,
  template: `
    @if (data.message) {
      <p>{{ data.message }}</p>
    }
    @if (single) {
      <app-action-success-single [single]="single" />
    }
    @if (group) {
      <app-action-success-group [group]="group" />
    }
  `,
  imports: [ActionSuccessGroupComponent, ActionSuccessSingleComponent],
})
export class ActionSuccessPage {
  readonly data =
    inject<TuiDialogContext<void, ActionResponseWithResult>>(
      POLYMORPHEUS_CONTEXT,
    ).data

  readonly single = this.data.result.type === 'single' ? this.data.result : null
  readonly group = this.data.result.type === 'group' ? this.data.result : null
}
