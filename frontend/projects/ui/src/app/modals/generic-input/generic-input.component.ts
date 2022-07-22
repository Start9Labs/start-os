import { Component, Input, ViewChild } from '@angular/core'
import { ModalController, IonicSafeString, IonInput } from '@ionic/angular'
import { getErrorMessage } from '@start9labs/shared'
import { MaskPipe } from 'src/app/pipes/mask/mask.pipe'

@Component({
  selector: 'generic-input',
  templateUrl: './generic-input.component.html',
  styleUrls: ['./generic-input.component.scss'],
  providers: [MaskPipe],
})
export class GenericInputComponent {
  @ViewChild('mainInput') elem?: IonInput

  @Input() options!: GenericInputOptions

  value!: string
  masked!: boolean

  maskedValue?: string

  error: string | IonicSafeString = ''

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly mask: MaskPipe,
  ) {}

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

    this.masked = !!this.options.useMask
    this.value = this.options.initialValue || ''
  }

  ngAfterViewInit() {
    setTimeout(() => this.elem?.setFocus(), 400)
  }

  toggleMask() {
    this.masked = !this.masked
  }

  cancel() {
    this.modalCtrl.dismiss()
  }

  transformInput(newValue: string) {
    let i = 0
    this.value = newValue
      .split('')
      .map(x => (x === '●' ? this.value[i++] : x))
      .join('')
    this.maskedValue = this.mask.transform(this.value)
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
