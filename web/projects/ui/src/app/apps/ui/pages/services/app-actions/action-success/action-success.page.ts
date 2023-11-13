import { Component, Inject } from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { ActionResponse } from 'src/app/services/api/api.types'

@Component({
  selector: 'action-success',
  templateUrl: './action-success.page.html',
})
export class ActionSuccessPage {
  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, ActionResponse>,
    readonly copyService: CopyService,
  ) {}

  get actionRes(): ActionResponse {
    return this.context.data
  }
}
