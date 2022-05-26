import { Component } from '@angular/core'
import { LoadingController, getPlatforms } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { Router } from '@angular/router'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
})
export class LoginPage {
  password = ''
  unmasked = false
  error = ''
  loader?: HTMLIonLoadingElement

  constructor(
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
  ) {}

  ngOnDestroy() {
    this.loader?.dismiss()
  }

  toggleMask() {
    this.unmasked = !this.unmasked
  }

  async submit() {
    this.error = ''

    this.loader = await this.loadingCtrl.create({
      message: 'Logging in',
      spinner: 'lines',
      cssClass: 'loader',
    })
    await this.loader.present()

    try {
      document.cookie = ''
      await this.api.login({
        password: this.password,
        metadata: { platforms: getPlatforms() },
      })

      this.password = ''
      this.authService
        .setVerified()
        .then(() => this.router.navigate([''], { replaceUrl: true }))
    } catch (e: any) {
      this.error = e.code === 34 ? 'Invalid Password' : e.message
    } finally {
      this.loader.dismiss()
    }
  }
}
