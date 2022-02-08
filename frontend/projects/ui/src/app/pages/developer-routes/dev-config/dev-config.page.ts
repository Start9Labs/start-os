import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import * as yaml from 'js-yaml'
import { GenericFormPage } from '../../../modals/generic-form/generic-form.page'
import { ConfigSpec } from '../../../pkg-config/config-types'
import { ErrorToastService } from '../../../services/error-toast.service'

@Component({
  selector: 'dev-config',
  templateUrl: 'dev-config.page.html',
  styleUrls: ['dev-config.page.scss'],
})
export class DevConfigPage {
  editorOptions = { theme: 'vs-dark', language: 'yaml' }
  code: string

  constructor(
    private readonly errToast: ErrorToastService,
    private readonly modalCtrl: ModalController,
  ) {}

  ngOnInit() {
    this.code = yaml
      .dump(SAMPLE_CODE)
      .replace(/warning:/g, '# Optional\n  warning:')
  }

  async submit() {
    let doc: any
    try {
      doc = yaml.load(this.code)
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

const SAMPLE_CODE: ConfigSpec = {
  'sample-string': {
    type: 'string',
    name: 'Example String Input',
    nullable: false,
    masked: false,
    copyable: false,
    // optional
    warning: null,
    description: 'Example description for required string input.',
    default: null,
    placeholder: 'Enter string value',
    pattern: '^[a-zA-Z0-9! _]+$',
    'pattern-description': 'Must be alphanumeric (may contain underscore).',
  },
  'sample-number': {
    type: 'number',
    name: 'Example Number Input',
    nullable: false,
    range: '[5,1000000]',
    integral: true,
    // optional
    warning: 'Example warning to display when changing this number value.',
    units: 'ms',
    description: 'Example description for optional number input.',
    default: null,
    placeholder: 'Enter number value',
  },
  'sample-boolean': {
    type: 'boolean',
    name: 'Example Boolean Toggle',
    // optional
    warning: null,
    description: 'Example description for boolean toggle',
    default: true,
  },
  'sample-enum': {
    type: 'enum',
    name: 'Example Enum Select',
    values: ['red', 'blue', 'green'],
    'value-names': {
      red: 'Red',
      blue: 'Blue',
      green: 'Green',
    },
    // optional
    warning: 'Example warning to display when changing this enum value.',
    description: 'Example description for enum select',
    default: 'red',
  },
}
