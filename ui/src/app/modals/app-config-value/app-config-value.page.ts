import { Component, Input } from '@angular/core'
import { getDefaultConfigValue, getDefaultDescription, Range } from 'src/app/pkg-config/config-utilities'
import { AlertController, LoadingController, ModalController, ToastController } from '@ionic/angular'
import { ConfigCursor } from 'src/app/pkg-config/config-cursor'
import { ValueSpecOf } from 'src/app/pkg-config/config-types'
import { copyToClipboard } from 'src/app/util/web.util'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-config-value',
  templateUrl: 'app-config-value.page.html',
  styleUrls: ['app-config-value.page.scss'],
})
export class AppConfigValuePage {
  @Input() cursor: ConfigCursor<'string' | 'number' | 'boolean' | 'enum'>
  @Input() saveFn?: (value: string | number | boolean) => Promise<any>

  spec: ValueSpecOf<'string' | 'number' | 'boolean' | 'enum'>
  value: string | number | boolean | null

  edited: boolean
  error: string
  unmasked = false

  defaultDescription: string
  integralDescription = 'Value must be a whole number.'

  range: Range
  rangeDescription: string

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly toastCtrl: ToastController,
    private readonly errToast: ErrorToastService,
  ) { }

  ngOnInit () {
    this.spec  = this.cursor.spec()
    this.value = this.cursor.config()
    this.error = this.cursor.checkInvalid()

    this.defaultDescription = getDefaultDescription(this.spec)
    if (this.spec.type === 'number') {
      this.range = Range.from(this.spec.range)
      this.rangeDescription = this.range.description()
    }
  }

  async dismiss () {
    if (this.value === '') this.value = null

    if (this.spec.type === 'number' && this.value !== null) {
      this.value = Number(this.value)
    }

    if ((!!this.saveFn && this.edited) || (!this.saveFn && this.error)) {
      await this.presentAlert()
    } else {
      await this.modalCtrl.dismiss(this.value)
    }
  }

  async save () {
    if (this.value === '') this.value = null

    if (this.spec.type === 'number' && this.value !== null) {
      this.value = Number(this.value)
    }

    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Saving...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.saveFn(this.value)
      this.modalCtrl.dismiss(this.value)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  refreshDefault () {
    this.value = getDefaultConfigValue(this.spec) as any
    this.handleInput()
  }

  handleInput () {
    this.validate()
    this.edited = true
  }

  clear () {
    this.value = null
    this.edited = true
  }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  async copy (): Promise<void> {
    let message = ''
    await copyToClipboard(String(this.value)).then(success => { message = success ? 'copied to clipboard!' :  'failed to copy'})

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  private validate (): boolean {
    if (this.spec.type === 'boolean') return true

    // test blank
    if (this.value === '' && !(this.spec as any).nullable) {
      this.error = 'Value cannot be blank'
      return false
    }
    // test pattern if string
    if (this.spec.type === 'string' && this.value) {
      const { pattern, patternDescription } = this.spec
      if (pattern && !RegExp(pattern).test(this.value as string)) {
        this.error = patternDescription || `Must match ${pattern}`
        return false
      }
    }
    // test range if number
    if (this.spec.type === 'number' && this.value) {
      if (this.spec.integral && !RegExp(/^[-+]?[0-9]+$/).test(String(this.value))) {
        this.error = this.integralDescription
        return false
      } else if (!this.spec.integral && !RegExp(/^[0-9]*\.?[0-9]+$/).test(String(this.value))) {
        this.error = 'Value must be a number.'
        return false
      } else {
        try {
          this.range.checkIncludes(Number(this.value))
        } catch (e) {
          console.warn(e) //an invalid spec is not an error
          this.error = e.message
          return false
        }
      }
    }

    this.error = ''
    return true
  }

  private async presentAlert () {
    const header = this.error ?
      'Invalid Entry' :
      'Unsaved Changes'

    const message = this.error ?
    'Value will not be saved' :
    'You have unsaved changes. Are you sure you want to leave?'

    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header,
      message,
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: `Leave`,
          cssClass: 'alert-danger',
          handler: () => {
            this.modalCtrl.dismiss()
          },
        },
      ],
    })
    await alert.present()
  }
}

