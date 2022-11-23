import { Component } from '@angular/core'
import { LoadingController, getPlatforms } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { Router } from '@angular/router'
import { ConfigService } from 'src/app/services/config.service'

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
  needsEncryption = !window.isSecureContext && !this.config.isTor()

  constructor(
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly config: ConfigService,
  ) {}

  async ionViewDidEnter() {
    if (this.needsEncryption) {
      try {
        await this.api.getPubKey()
      } catch (e: any) {
        this.error = e
      }
    }
  }

  ngOnDestroy() {
    this.loader?.dismiss()
  }

  toggleMask() {
    this.unmasked = !this.unmasked
  }

  async submit() {
    this.error = ''

    this.loader = await this.loadingCtrl.create({
      message: 'Logging in...',
    })
    await this.loader.present()

    try {
      document.cookie = ''
      await this.api.login({
        password: this.needsEncryption
          ? await this.api.encrypt(this.password)
          : this.password,
        metadata: { platforms: getPlatforms() },
      })

      this.password = ''
      this.authService.setVerified()
      this.router.navigate([''], { replaceUrl: true })
    } catch (e: any) {
      // code 7 is for incorrect password
      this.error = e.code === 7 ? 'Invalid Password' : e.message
    } finally {
      this.loader.dismiss()
    }
  }
}
