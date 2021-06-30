import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'password',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  @Input() version: string | null

  needsVer = true

  error = ''
  password = ''
  passwordVer = ''

  constructor(
    private modalController: ModalController
  ) {}

  ngOnInit() {
    this.needsVer = !this.version?.startsWith('0.3')
    console.log('needs', this.needsVer)
  }

  async submitPassword () {
    if (this.needsVer && this.password !== this.passwordVer) {
      this.error="*passwords dont match"
    } else {
      this.dismiss(true)
    }
  }

  cancel () {
    this.dismiss(false)
  }

  dismiss(submitted: boolean) {
    this.modalController.dismiss({
      pwValid: submitted,
      pw: this.password
    });
  }

}
