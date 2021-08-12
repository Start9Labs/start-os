import { Component } from '@angular/core'
import { LoadingController } from '@ionic/angular'
import { Subscription } from 'rxjs'
import { AuthService } from 'src/app/services/auth.service'
import { PatchConnection, PatchDbService } from 'src/app/services/patch-db/patch-db.service'

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
    private readonly patch: PatchDbService,
  ) { }

  ngOnInit () {

  }

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
      this.loader.message = 'Loading Embassy Data'
      this.password = ''
      this.patchConnectionSub = this.patch.watchPatchConnection$().subscribe(status => {
        if (status === PatchConnection.Disconnected) {
          this.error = 'Connection failed'
          this.loader.dismiss()
        }
      })
    } catch (e) {
      this.error = e.message
      this.loader.dismiss()
    }
  }
}
