import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService, EmbassyDrive, RecoveryDrive } from 'src/app/services/api/api.service'

@Component({
  selector: 'password',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  @Input() recoveryDrive: RecoveryDrive
  @Input() embassyDrive: EmbassyDrive
  @Input() verify: boolean

  error = ''
  password = ''
  passwordVer = ''

  constructor(
    private modalController: ModalController,
    private apiService: ApiService,
    private loadingCtrl: LoadingController
  ) {}

  ngOnInit() { }

  async verifyPw () {
    
    if(!this.recoveryDrive) this.error = 'No recovery drive' // unreachable
    const loader = await this.loadingCtrl.create({
      message: 'Verifying Password'
    })
    await loader.present()

    try {
      const isCorrectPassword = await this.apiService.verifyRecoveryPassword(this.recoveryDrive.logicalname, this.password)
      if(isCorrectPassword) {
        this.modalController.dismiss({ password: this.password })
      } else {
        this.error = "Incorrect password provided"
      }
    } catch (e) {
      this.error = 'Error connecting to Embassy'
    } finally {
      loader.dismiss()
    }
  }

  async submitPw () {
    this.validate()
    if(this.error) return


  }

  validate () {
    if (this.password.length < 12) {
      this.error="*passwords must be 12 characters or greater"
    } else if (this.password !== this.passwordVer) {
      this.error="*passwords dont match"
    } else {
      this.error = ''
    }
  }


  cancel () {
    this.modalController.dismiss()
  }
}
