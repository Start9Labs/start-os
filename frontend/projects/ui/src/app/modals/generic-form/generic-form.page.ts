import { Component, Input } from '@angular/core'
import { UntypedFormGroup } from '@angular/forms'
import { ModalController } from '@ionic/angular'
import {
  convertValuesRecursive,
  FormService,
} from 'src/app/services/form.service'
import { InputSpec } from 'start-sdk/types/config-types'

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

    // @TODO make this more like generic input component dismissal
    const success = await handler(this.formGroup.value)
    if (success === true) this.modalCtrl.dismiss()
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
