import { Component, ElementRef, Input, ViewChild } from '@angular/core'
import { FormGroup } from '@angular/forms'
import { AlertController, ModalController } from '@ionic/angular'
import { Action } from 'src/app/services/patch-db/data-model'
import { FormService } from 'src/app/services/form.service'

@Component({
  selector: 'app-action-input',
  templateUrl: './app-action-input.page.html',
  styleUrls: ['./app-action-input.page.scss'],
})
export class AppActionInputPage {
  @Input() action: Action
  actionForm: FormGroup
  current = {
    'bitcoinNode': {
      'type': 'external',
      'public-domain': 'My Node',
    },
    // 'days-ago': 100,
    'emergency-contact': {
        'email': 'matt@test.com',
        'name': 'Matt',
    },
    'ips': [
        '19',
    ],
    'notifications': [
        'email',
        'text',
    ],
    'profile': {
        'email': null,
        // 'firstName': 'Drew',
        'lastName': 'Ans',
        'password': 'testestpass',
    },
    'randomEnum': 'bad',
    'reason': 'TestingTesting',
    'testnet': true,
  }

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) { }

  ngOnInit () {
    this.actionForm = this.formService.createForm(this.action['input-spec'], this.current)
  }

  async dismiss (): Promise<void> {
    this.modalCtrl.dismiss()
  }

  async save (): Promise<void> {
    if (this.actionForm.invalid) {
      this.actionForm.markAllAsTouched()
      document.getElementsByClassName('validation-error')[0].parentElement.parentElement.scrollIntoView({ behavior: 'smooth' })
      return
    }
    this.modalCtrl.dismiss(this.actionForm.value)
  }

  asIsOrder () {
    return 0
  }
}
