import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnInit,
} from '@angular/core'
import { FormService } from 'src/app/services/form.service'
import { InputSpec } from 'start-sdk/lib/config/configTypes'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'
import { tuiMarkControlAsTouchedAndValidate } from '@taiga-ui/cdk'
import { InvalidService } from '../../components/form/invalid.service'
import { TuiDialogFormService } from '@taiga-ui/kit'
import { FormGroup } from '@angular/forms'

export interface ActionButton<T> {
  text: string
  handler: (value: T) => Promise<boolean>
}

export interface FormContext<T> {
  spec: InputSpec
  buttons: ActionButton<T>[]
  value?: T
}

@Component({
  selector: 'form-page',
  templateUrl: './form.page.html',
  styleUrls: ['./form.page.scss'],
  providers: [InvalidService],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormPage<T extends Record<string, any>> implements OnInit {
  private readonly dialogFormService = inject(TuiDialogFormService)
  private readonly formService = inject(FormService)
  private readonly invalidService = inject(InvalidService)
  private readonly context = inject<TuiDialogContext<void, FormContext<T>>>(
    POLYMORPHEUS_CONTEXT,
    { optional: true },
  )

  @Input() spec = this.context?.data.spec || {}
  @Input() buttons = this.context?.data.buttons || []
  @Input() value?: T = this.context?.data.value

  form = new FormGroup({})

  ngOnInit() {
    this.form = this.formService.createForm(this.spec, this.value)
  }

  async onClick(handler: ActionButton<T>['handler']) {
    tuiMarkControlAsTouchedAndValidate(this.form)
    this.invalidService.scrollIntoView()

    if (this.form.valid && (await handler(this.form.value as T))) {
      this.context?.$implicit.complete()
    }
  }

  markAsDirty() {
    this.dialogFormService.markAsDirty()
  }
}
