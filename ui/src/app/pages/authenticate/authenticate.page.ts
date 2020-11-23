import { Component, OnInit } from '@angular/core'
import { AuthService } from '../../services/auth.service'
import { LoaderService } from '../../services/loader.service'
import { BehaviorSubject } from 'rxjs'
import { Router } from '@angular/router'

@Component({
  selector: 'app-authenticate',
  templateUrl: './authenticate.page.html',
  styleUrls: ['./authenticate.page.scss'],
})
export class AuthenticatePage implements OnInit {
  password: string = ''
  unmasked = false
  $error$ = new BehaviorSubject(undefined)

  constructor (
    private readonly authStore: AuthService,
    private readonly loader: LoaderService,
    private readonly router: Router,
  ) { }

  ngOnInit () { }

  ionViewDidEnter () {
    this.$error$.next(undefined)
  }

  toggleMask () {
    this.unmasked = !this.unmasked
  }

  async submitPassword () {
    try {
      await this.loader.displayDuringP(
        this.authStore.login(this.password),
      )
      this.password = ''
      return this.router.navigate([''])
    } catch (e) {
      this.$error$.next(e.message)
    }
  }
}
