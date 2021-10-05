import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'generic-input',
  templateUrl: './generic-input.component.html',
  styleUrls: ['./generic-input.component.scss'],
})
export class GenericInputComponent {
  @Input() title: string
  @Input() message: string
  @Input() label: string
  @Input() buttonText = 'Submit'
  @Input() placeholder = 'Enter Value'
  @Input() nullable = false
  @Input() useMask = false
  @Input() value = ''
  @Input() submitFn: (value: string) => Promise<any>
  unmasked = false
  error: string

  constructor (
    private readonly modalCtrl: ModalController,
  ) { }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  cancel () {
    this.modalCtrl.dismiss()
  }

  async submit () {
    // @TODO validate input?
    await this.submitFn(this.value)
    this.modalCtrl.dismiss(undefined, 'success')
  }
}
