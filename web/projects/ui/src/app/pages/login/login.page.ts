import { DOCUMENT } from '@angular/common'
import { Component, Inject } from '@angular/core'
import { Router } from '@angular/router'
import { getPlatforms } from '@ionic/angular'
import { LoadingService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
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

  constructor(
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    public readonly config: ConfigService,
    @Inject(DOCUMENT) public readonly document: Document,
  ) {}

  async submit() {
    this.error = ''

    const loader = this.loader.open('Logging in...').subscribe()

    try {
      document.cookie = ''
      if (this.password.length > 64) {
        this.error = 'Password must be less than 65 characters'
        return
      }
      await this.api.login({
        password: this.password,
        metadata: { platforms: getPlatforms() },
        ephemeral: window.location.host === 'localhost',
      })

      this.password = ''
      this.authService.setVerified()
      this.router.navigate([''], { replaceUrl: true })
    } catch (e: any) {
      // code 7 is for incorrect password
      this.error = e.code === 7 ? 'Invalid Password' : e.message
    } finally {
      loader.unsubscribe()
    }
  }
}
