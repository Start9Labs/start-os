import { Component, Input } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { ApiService, EmbassyDrive, RecoveryDrive } from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'password',
  templateUrl: 'password.page.html',
  styleUrls: ['password.page.scss'],
})
export class PasswordPage {
  @Input() recoveryDrive: RecoveryDrive
  @Input() embassyDrive: EmbassyDrive

  error = ''
  password = ''
  passwordVer = ''

  constructor(
    private modalController: ModalController,
    private apiService: ApiService,
    private loadingCtrl: LoadingController,
    private stateService: StateService
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
    if (!this.error && this.password !== this.passwordVer) {
      this.error="*passwords dont match"
    }

    if(this.error) return
    const loader = await this.loadingCtrl.create({
      message: 'Setting up your Embassy!'
    })
    
    await loader.present()


    this.stateService.embassyDrive = this.embassyDrive
    this.stateService.embassyPassword = this.password

    try {
      await this.stateService.setupEmbassy()
      this.modalController.dismiss({ success: true })
    } catch (e) {
      this.error = e.message
    } finally {
      loader.dismiss()
    }
  }

  validate () {
    if (this.password.length < 12) {
      this.error="*password must be 12 characters or greater"
    } else {
      this.error = ''
    }
  }


  cancel () {
    this.modalController.dismiss()
  }
}
