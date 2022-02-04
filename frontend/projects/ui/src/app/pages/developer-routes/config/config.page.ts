import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import * as yaml from 'js-yaml'
import { GenericFormPage } from '../../../modals/generic-form/generic-form.page'
import { ErrorToastService } from '../../../services/error-toast.service'
import { isObject } from '../../../util/misc.util'

@Component({
  selector: 'config',
  templateUrl: 'config.page.html',
  styleUrls: ['config.page.scss'],
})
export class ConfigPage {
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
    } catch (e) {
      this.errToast.present({
        message: `Error parsing YAML on line ${findError(this.code)}`,
      } as any)
      throw e
    }

    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Config Sample',
        spec: camelToKabab(JSON.parse(JSON.stringify(doc, null, 2))),
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

const SAMPLE_CODE = `name:
  type: string
  name: Alias
  description: The public, human-readable name of your Lightning node
  nullable: true
  pattern: ".{1,32}"
  patternDescription: Must be at least 1 character and no more than 32 characters
color:
  type: string
  name: Color
  description: The public color dot of your Lightning node
  nullable: false
  pattern: "[0-9a-fA-F]{6}"
  patternDescription: |
    Must be a valid 6 digit hexadecimal RGB value. The first two digits are red, middle two are green, and final two are
    blue
  default:
    charset: "a-f,0-9"
    len: 6`

function camelToKabab(obj: {}) {
  const ret = {}
  Object.keys(obj).forEach(key => {
    const val = obj[key]
    const newKey = key.replace(/[A-Z]/g, letter => `-${letter.toLowerCase()}`)
    if (isObject(val)) {
      ret[newKey] = camelToKabab(val)
    } else {
      ret[newKey] = val
    }
  })
  return ret
}

function findError(y: string, prevMin = 0, prevMax: number = 0): number {
  if (prevMax - prevMin === 1) return prevMax

  const lines = y.split(/\r\n|\r|\n/)
  if (!prevMax) prevMax = lines.length

  const length = Math.ceil((prevMin + prevMax) / 2)
  const testYaml = lines.slice(0, length).join('\n')

  let newMin = length
  let newMax = prevMax

  try {
    yaml.load(testYaml)
  } catch (e) {
    newMin = prevMin
    newMax = length
  }

  return findError(y, newMin, newMax)
}
