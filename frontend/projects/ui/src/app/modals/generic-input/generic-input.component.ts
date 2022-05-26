import { Component, Input, ViewChild } from '@angular/core'
import { ModalController, IonicSafeString, IonInput } from '@ionic/angular'
import { getErrorMessage } from '@start9labs/shared'

@Component({
  selector: 'generic-input',
  templateUrl: './generic-input.component.html',
  styleUrls: ['./generic-input.component.scss'],
})
export class GenericInputComponent {
  @ViewChild('mainInput') elem: IonInput
  @Input() options: GenericInputOptions
  value: string
  unmasked = false
  error: string | IonicSafeString

  constructor(private readonly modalCtrl: ModalController) {}

  ngOnInit() {
    const defaultOptions: Partial<GenericInputOptions> = {
      buttonText: 'Submit',
      placeholder: 'Enter value',
      nullable: false,
      useMask: false,
      initialValue: '',
    }
    this.options = {
      ...defaultOptions,
      ...this.options,
    }

    this.value = this.options.initialValue
  }

  ngAfterViewInit() {
    setTimeout(() => this.elem.setFocus(), 400)
  }

  toggleMask() {
    this.unmasked = !this.unmasked
  }

  cancel() {
    this.modalCtrl.dismiss()
  }

  async submit() {
    const value = this.value.trim()

    if (!value && !this.options.nullable) return

    try {
      await this.options.submitFn(value)
      this.modalCtrl.dismiss(undefined, 'success')
    } catch (e: any) {
      this.error = getErrorMessage(e)
    }
  }
}

export interface GenericInputOptions {
  // required
  title: string
  message: string
  label: string
  submitFn: (value: string) => Promise<any>
  // optional
  warning?: string
  buttonText?: string
  placeholder?: string
  nullable?: boolean
  useMask?: boolean
  initialValue?: string
}
