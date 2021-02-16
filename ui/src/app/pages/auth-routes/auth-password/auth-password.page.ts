import { Component } from '@angular/core'
import { AuthService } from 'src/app/services/auth.service'
import { LoaderService } from 'src/app/services/loader.service'
import { NavController } from '@ionic/angular'

@Component({
  selector: 'auth-password',
  templateUrl: './auth-password.page.html',
  styleUrls: ['./auth-password.page.scss'],
})
export class AuthPasswordPage {
  password: string = ''
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
        this.authService.submitPassword(this.password),
      )
      this.password = ''
      return this.navCtrl.navigateForward([''])
    } catch (e) {
      this.error = e.message
    }
  }
}
