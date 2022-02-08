import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import * as yaml from 'js-yaml'
import { GenericFormPage } from '../../../modals/generic-form/generic-form.page'
import { ErrorToastService } from '../../../services/error-toast.service'

@Component({
  selector: 'dev-config',
  templateUrl: 'dev-config.page.html',
  styleUrls: ['dev-config.page.scss'],
})
export class DevConfigPage {
  editorOptions = { theme: 'vs-dark', language: 'yaml' }
  code: string = SAMPLE_CODE

  constructor(
    private readonly errToast: ErrorToastService,
    private readonly modalCtrl: ModalController,
  ) {}

  async submit() {
    let doc: string
    try {
      doc = yaml.load(this.code)
      console.log(JSON.parse(JSON.stringify(doc, null, 2)))
    } catch (e) {
      this.errToast.present(e)
    }

    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Config Sample',
        spec: JSON.parse(JSON.stringify(doc, null, 2)),
        buttons: [
          {
            text: 'OK',
            handler: () => {
              return
            },
            isSubmit: true,
          },
        ],
      },
    })
    await modal.present()
  }
}

const SAMPLE_CODE = `sample-input:
  type: string
  nullable: false
  name: "Example"
  description: "This is an example string input for a config"
  default: "Welcome to config!"
  pattern: "^[a-zA-Z0-9! _]+$"
  pattern-description: "Must be alphanumeric (can contain underscore)."
favenum:
  name: Favorite Number
  description: Whats your favorite number?  We're dying to know
  type: number
  nullable: false
  range: "[5,1000000]"
  integral: true
  default: 30
friends:
  type: boolean
  name: Are we friends?
  description: I could really use a ride to the airport this weekend
  default: false
favecolor:
  name: Favorite Color
  description: Select your favorite color.  Unfortunately your options are pretty limited
  type: enum
  values:
    - red
    - blue
    - green
  value-names:
    red: Red
    blue: Blue
    green: Green
  default: blue`
