import { Component, Input } from '@angular/core'
import { UntypedFormGroup } from '@angular/forms'
import { ModalController } from '@ionic/angular'
import {
  convertValuesRecursive,
  FormService,
} from 'src/app/services/form.service'
import { InputSpec } from '@start9labs/start-sdk/lib/config/configTypes'
import { ErrorToastService } from '@start9labs/shared'

export interface ActionButton {
  text: string
  handler: (value: any) => Promise<boolean>
  isSubmit?: boolean
}

@Component({
  selector: 'generic-form',
  templateUrl: './generic-form.page.html',
  styleUrls: ['./generic-form.page.scss'],
})
export class GenericFormPage {
  @Input() title!: string
  @Input() spec!: InputSpec
  @Input() buttons!: ActionButton[]
  @Input() initialValue: Record<string, any> = {}

  submitBtn!: ActionButton
  formGroup!: UntypedFormGroup

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
    private readonly errToast: ErrorToastService,
  ) {}

  ngOnInit() {
    this.formGroup = this.formService.createForm(this.spec, this.initialValue)
    this.submitBtn = this.buttons.find(btn => btn.isSubmit)! // @TODO this really needs to be redesigned. No way to enforce this with types.
  }

  async dismiss(): Promise<void> {
    this.modalCtrl.dismiss()
  }

  async handleClick(handler: ActionButton['handler']): Promise<void> {
    convertValuesRecursive(this.spec, this.formGroup)

    if (this.formGroup.invalid) {
      document
        .getElementsByClassName('validation-error')[0]
        ?.scrollIntoView({ behavior: 'smooth' })
      return
    }

    try {
      const response = await handler(this.formGroup.value)
      this.modalCtrl.dismiss({ response }, 'success')
    } catch (e: any) {
      this.errToast.present(e)
    }
  }
}

export interface GenericFormOptions {
  // required
  title: string
  spec: InputSpec
  buttons: ActionButton[]
  // optional
  initialValue?: Record<string, any>
}
