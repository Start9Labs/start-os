import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'

@Component({
  selector: 'prompt',
  templateUrl: 'prompt.component.html',
  styleUrls: ['prompt.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PromptComponent {
  masked = this.options.useMask
  value = this.options.initialValue || ''

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<string, PromptOptions>,
  ) {}

  get options(): PromptOptions {
    return this.context.data
  }

  cancel() {
    this.context.$implicit.complete()
  }

  submit(value: string) {
    if (value || !this.options.required) {
      this.context.$implicit.next(value)
    }
  }
}

export const PROMPT = new PolymorpheusComponent(PromptComponent)

export interface PromptOptions {
  message: string
  label?: string
  warning?: string
  buttonText?: string
  placeholder?: string
  required?: boolean
  useMask?: boolean
  initialValue?: string | null
}
