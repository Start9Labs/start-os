import { Component } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { Subscription } from 'rxjs'
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
  patchConnectionSub: Subscription

  constructor (
    private readonly authService: AuthService,
    private readonly loadingCtrl: LoadingController,
  ) { }

  ngOnDestroy () {
    if (this.loader) {
      this.loader.dismiss()
      this.loader = undefined
    }
    if (this.patchConnectionSub) {
      this.patchConnectionSub.unsubscribe()
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
      cssClass: 'loader',
    })
    await this.loader.present()

    try {
      await this.authService.login(this.password)
      this.password = ''
    } catch (e) {
      this.error = e.code === 34 ? 'Invalid Password' : e.message
    } finally {
      this.loader.dismiss()
    }
  }
}
