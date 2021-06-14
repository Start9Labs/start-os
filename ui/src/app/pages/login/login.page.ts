import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import { AuthService } from 'src/app/services/auth.service'
import { LoaderService } from 'src/app/services/loader.service'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
})
export class LoginPage {
  password = ''
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
        this.authService.login(this.password),
      )
      this.password = ''
      await this.navCtrl.navigateForward(['/'])
    } catch (e) {
      this.error = e.message
    }
  }
}
