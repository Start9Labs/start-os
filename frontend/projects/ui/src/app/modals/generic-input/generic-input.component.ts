import { Component, inject, Input, ViewChild } from '@angular/core'
import { ModalController, IonicSafeString, IonInput } from '@ionic/angular'
import { getErrorMessage, THEME } from '@start9labs/shared'
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

  readonly theme$ = inject(THEME)

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly mask: MaskPipe,
  ) {}

  ngOnInit() {
    const defaultOptions: Partial<GenericInputOptions> = {
      buttonText: 'Submit',
      required: true,
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

  async submit<T>() {
    const value = this.value.trim()

    if (!value && this.options.required) return

    try {
      const response = await this.options.submitFn(value)
      this.modalCtrl.dismiss({ response, value }, 'success')
    } catch (e: any) {
      this.error = getErrorMessage(e)
    }
  }
}

export interface GenericInputOptions {
  // required
  title: string
  message: string
  submitFn: (value: string) => Promise<any>
  // optional
  label?: string
  warning?: string
  buttonText?: string
  placeholder?: string
  required?: boolean
  useMask?: boolean
  initialValue?: string | null
}
