import { Component } from '@angular/core'
import { AlertController, NavController } from '@ionic/angular'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.sevice'

@Component({
  selector: 'password-page',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  error = ''
  password = ''
  passwordVer = ''

  constructor(
    private readonly apiService: ApiService,
    private readonly stateService: StateService,
    public alertController: AlertController,
  ) {}

  async submitPassword () {
    if (!this.stateService.recoveryDrive && this.password !== this.passwordVer) {
      console.log('here')
      this.error="*passwords dont match"
    } else {
      console.log('submitting')
      await this.apiService.submitPassword(this.password)
      // @Todo navigate to embassy os
    }
  }

}
