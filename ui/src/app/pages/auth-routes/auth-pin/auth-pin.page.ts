import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { AuthService } from 'src/app/services/auth.service'
import { LoaderService } from 'src/app/services/loader.service'

@Component({
  selector: 'auth-pin',
  templateUrl: './auth-pin.page.html',
  styleUrls: ['./auth-pin.page.scss'],
})
export class AuthPinPage {
  pin = ''
  unmasked = false
  error = ''

  constructor (
    private readonly authService: AuthService,
    private readonly loader: LoaderService,
    private readonly navCtrl: NavController,
  ) { }

  ionViewDidEnter () {
    this.error = ''
  }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  async submit () {
    try {
      await this.loader.displayDuringP(
        this.authService.submitPin(this.pin),
      )
      this.pin = ''
      await this.navCtrl.navigateForward(['/auth/password'])
    } catch (e) {
      this.error = e.message
    }
  }
}
