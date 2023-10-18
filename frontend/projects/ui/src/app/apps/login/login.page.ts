import { Component, Inject } from '@angular/core'
import { getPlatforms, LoadingController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { Router } from '@angular/router'
import { ConfigService } from 'src/app/services/config.service'
import { DOCUMENT } from '@angular/common'
import { WINDOW } from '@ng-web-apis/common'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
})
export class LoginPage {
  password = ''
  unmasked = false
  error = ''

  constructor(
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    public readonly config: ConfigService,
    @Inject(DOCUMENT) public readonly document: Document,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  launchHttps() {
    const host = this.config.getHost()
    this.windowRef.open(`https://${host}`, '_blank', 'noreferrer')
  }

  async submit() {
    this.error = ''

    const loader = await this.loadingCtrl.create({
      message: 'Logging in...',
    })
    await loader.present()

    try {
      document.cookie = ''
      if (this.password.length > 64) {
        this.error = 'Password must be less than 65 characters'
        return
      }
      await this.api.login({
        password: this.password,
        metadata: { platforms: getPlatforms() },
      })

      this.password = ''
      this.authService.setVerified()
      this.router.navigate([''], { replaceUrl: true })
    } catch (e: any) {
      // code 7 is for incorrect password
      this.error = e.code === 7 ? 'Invalid Password' : e.message
    } finally {
      loader.dismiss()
    }
  }
}
