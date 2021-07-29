import { Component } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { AuthService } from 'src/app/services/auth.service'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
})
export class LoginPage {
  password = ''
  unmasked = false
  error = ''
  loader: HTMLIonLoadingElement

  constructor (
    private readonly authService: AuthService,
    private readonly loadingCtrl: LoadingController,
  ) { }

  ngOnDestroy () {
    if (this.loader) {
      this.loader.dismiss()
      this.loader = undefined
    }
  }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  async submit () {
    this.error = ''

    this.loader = await this.loadingCtrl.create({
      message: 'Logging in',
      spinner: 'lines',
    })
    await this.loader.present()

    try {
      await this.authService.login(this.password)
      this.loader.message = 'Loading Embassy Data'
      this.password = ''
    } catch (e) {
      this.error = e.message
      this.loader.dismiss()
    }
  }
}
