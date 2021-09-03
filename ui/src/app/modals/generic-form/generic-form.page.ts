import { Component, Input } from '@angular/core'
import { FormGroup } from '@angular/forms'
import { ModalController } from '@ionic/angular'
import { FormService } from 'src/app/services/form.service'
import { ConfigSpec } from 'src/app/pkg-config/config-types'

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
  @Input() title: string
  @Input() spec: ConfigSpec
  @Input() buttons: ActionButton[]
  submitBtn: ActionButton
  formGroup: FormGroup

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) { }

  ngOnInit () {
    this.formGroup = this.formService.createForm(this.spec)
    this.submitBtn = this.buttons.find(btn => btn.isSubmit) || {
      text: '',
      handler: () => Promise.resolve(true),
    }
    console.log(this.submitBtn)
  }

  async dismiss (): Promise<void> {
    this.modalCtrl.dismiss()
  }

  async handleClick (handler: ActionButton['handler']): Promise<void> {
    if (this.formGroup.invalid) {
      this.formGroup.markAllAsTouched()
      document.getElementsByClassName('validation-error')[0].parentElement.parentElement.scrollIntoView({ behavior: 'smooth' })
      return
    }

    const success = await handler(this.formGroup.value)
    if (success !== false) this.modalCtrl.dismiss()
  }
}
