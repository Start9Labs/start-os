import { Component, Inject } from '@angular/core'
import { getPlatforms } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { Router } from '@angular/router'
import { ConfigService } from 'src/app/services/config.service'
import { LoadingService } from '@start9labs/shared'
import { TuiDestroyService } from '@taiga-ui/cdk'
import { takeUntil } from 'rxjs'
import { DOCUMENT } from '@angular/common'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
  providers: [TuiDestroyService],
})
export class LoginPage {
  password = ''
  unmasked = false
  error = ''
  secure = this.config.isSecure()

  constructor(
    @Inject(DOCUMENT) private readonly document: Document,
    private readonly destroy$: TuiDestroyService,
    private readonly router: Router,
    private readonly authService: AuthService,
    private readonly loader: LoadingService,
    private readonly api: ApiService,
    private readonly config: ConfigService,
  ) {}

  async ionViewDidEnter() {
    if (!this.secure) {
      try {
        await this.api.getPubKey()
      } catch (e: any) {
        this.error = e.message
      }
    }
  }

  toggleMask() {
    this.unmasked = !this.unmasked
  }

  async submit() {
    this.error = ''

    const loader = this.loader
      .open('Logging in...')
      .pipe(takeUntil(this.destroy$))
      .subscribe()

    try {
      this.document.cookie = ''
      if (this.password.length > 64) {
        this.error = 'Password must be less than 65 characters'
        return
      }
      await this.api.login({
        password: this.secure
          ? this.password
          : await this.api.encrypt(this.password),
        metadata: { platforms: getPlatforms() },
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
