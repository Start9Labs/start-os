import { Component } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'password',
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
