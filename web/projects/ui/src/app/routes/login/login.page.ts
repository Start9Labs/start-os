import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { Component, Inject, DestroyRef, inject } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { AuthService } from 'src/app/services/auth.service'
import { Router } from '@angular/router'
import { ConfigService } from 'src/app/services/config.service'
import { LoadingService } from '@start9labs/shared'
import { takeUntil } from 'rxjs'
import { DOCUMENT } from '@angular/common'

@Component({
  selector: 'login',
  templateUrl: './login.page.html',
  styleUrls: ['./login.page.scss'],
  providers: [],
})
export class LoginPage {
  password = ''
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

    const loader = this.loader
      .open('Logging in...')
      .pipe(takeUntilDestroyed(this.destroyRef))
      .subscribe()

    try {
      this.document.cookie = ''
      if (this.password.length > 64) {
        this.error = 'Password must be less than 65 characters'
        return
      }
      await this.api.login({
        password: this.password,
        // TODO: get platforms metadata
        metadata: { platforms: [] },
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

  readonly destroyRef = inject(DestroyRef)
}
