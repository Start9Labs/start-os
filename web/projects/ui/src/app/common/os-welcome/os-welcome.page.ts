import { Component, Inject } from '@angular/core'
import { TuiDialogContext } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'os-welcome',
  templateUrl: './os-welcome.page.html',
  styleUrls: ['./os-welcome.page.scss'],
})
export class OSWelcomePage {
  constructor(
    @Inject(POLYMORPHEUS_CONTEXT) readonly context: TuiDialogContext,
  ) {}
}
