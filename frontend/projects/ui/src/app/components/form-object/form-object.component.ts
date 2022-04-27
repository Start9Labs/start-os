import { Component, Input, Output, EventEmitter } from '@angular/core'
import {
  AbstractFormGroupDirective,
  FormArray,
  FormGroup,
} from '@angular/forms'
import {
  AlertButton,
  AlertController,
  IonicSafeString,
  ModalController,
} from '@ionic/angular'
import {
  ConfigSpec,
  ListValueSpecOf,
  ValueSpec,
  ValueSpecBoolean,
  ValueSpecList,
  ValueSpecListOf,
  ValueSpecUnion,
} from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'
import { Range } from 'src/app/pkg-config/config-utilities'
import { EnumListPage } from 'src/app/modals/enum-list/enum-list.page'
import { pauseFor } from '@start9labs/shared'
import { v4 } from 'uuid'
const Mustache = require('mustache')

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec: ConfigSpec
  @Input() formGroup: FormGroup
  @Input() unionSpec: ValueSpecUnion
  @Input() current: { [key: string]: any }
  @Input() showEdited: boolean = false
  @Output() onInputChange = new EventEmitter<void>()
  @Output() onExpand = new EventEmitter<void>()
  warningAck: { [key: string]: boolean } = {}
  unmasked: { [key: string]: boolean } = {}
  objectDisplay: { [key: string]: { expanded: boolean; height: string } } = {}
  objectListDisplay: {
    [key: string]: { expanded: boolean; height: string; displayAs: string }[]
  } = {}
  private objectId = v4()

  Object = Object

  constructor(
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) {}

  ngOnInit() {
    Object.keys(this.objectSpec).forEach(key => {
      const spec = this.objectSpec[key]

      if (spec.type === 'list' && ['object', 'union'].includes(spec.subtype)) {
        this.objectListDisplay[key] = []
        this.formGroup.get(key).value.forEach((obj, index) => {
          const displayAs = (spec.spec as ListValueSpecOf<'object'>)[
            'display-as'
          ]
          this.objectListDisplay[key][index] = {
            expanded: false,
            height: '0px',
            displayAs: displayAs
              ? (Mustache as any).render(displayAs, obj)
              : '',
          }
        })
      } else if (['object', 'union'].includes(spec.type)) {
        this.objectDisplay[key] = {
          expanded: false,
          height: '0px',
        }
      }
    })
  }

  getEnumListDisplay(arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec['value-names'][v]).join(', ')
  }

  updateUnion(e: any): void {
    const primary = this.unionSpec.tag.id

    Object.keys(this.formGroup.controls).forEach(control => {
      if (control === primary) return
      this.formGroup.removeControl(control)
    })

    const unionGroup = this.formService.getUnionObject(
      this.unionSpec as ValueSpecUnion,
      e.detail.value,
    )

    Object.keys(unionGroup.controls).forEach(control => {
      if (control === primary) return
      this.formGroup.addControl(control, unionGroup.controls[control])
    })

    Object.entries(this.unionSpec.variants[e.detail.value]).forEach(
      ([key, value]) => {
        if (['object', 'union'].includes(value.type)) {
          this.objectDisplay[key] = {
            expanded: false,
            height: '0px',
          }
        }
      },
    )

    this.onExpand.emit()
  }

  resize(key: string, i?: number): void {
    setTimeout(() => {
      if (i !== undefined) {
        this.objectListDisplay[key][i].height = this.getDocSize(key, i)
      } else {
        this.objectDisplay[key].height = this.getDocSize(key)
      }
      this.onExpand.emit()
    }, 250) // 250 to match transition-duration, defined in html
  }

  addListItemWrapper(key: string, spec: ValueSpec) {
    this.presentAlertChangeWarning(key, spec, () => this.addListItem(key))
  }

  addListItem(key: string, markDirty = true, val?: string): void {
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(listSpec, val)
    newItem.markAllAsTouched()
    arr.insert(0, newItem)
    if (['object', 'union'].includes(listSpec.subtype)) {
      const displayAs = (listSpec.spec as ListValueSpecOf<'object'>)[
        'display-as'
      ]
      this.objectListDisplay[key].unshift({
        height: '0px',
        expanded: true,
        displayAs: displayAs ? Mustache.render(displayAs, newItem.value) : '',
      })

      pauseFor(200).then(() => {
        this.objectListDisplay[key][0].height = this.getDocSize(key, 0)
        this.onExpand.emit()
      })
    }
  }

  toggleExpandObject(key: string) {
    this.objectDisplay[key].expanded = !this.objectDisplay[key].expanded
    this.objectDisplay[key].height = this.objectDisplay[key].expanded
      ? this.getDocSize(key)
      : '0px'
    this.onExpand.emit()
  }

  toggleExpandListObject(key: string, i: number) {
    this.objectListDisplay[key][i].expanded =
      !this.objectListDisplay[key][i].expanded
    this.objectListDisplay[key][i].height = this.objectListDisplay[key][i]
      .expanded
      ? this.getDocSize(key, i)
      : '0px'
    this.onExpand.emit()
  }

  updateLabel(key: string, i: number, displayAs: string) {
    this.objectListDisplay[key][i].displayAs = displayAs
      ? Mustache.render(displayAs, this.formGroup.get(key).value[i])
      : ''
  }

  getWarningText(text: string): IonicSafeString {
    if (text)
      return new IonicSafeString(`<ion-text color="warning">${text}</ion-text>`)
  }

  handleInputChange() {
    this.onInputChange.emit()
  }

  handleBooleanChange(key: string, spec: ValueSpecBoolean) {
    if (spec.warning) {
      const current = this.formGroup.get(key).value
      const cancelFn = () => this.formGroup.get(key).setValue(!current)
      this.presentAlertChangeWarning(key, spec, undefined, cancelFn)
    }
  }

  async presentModalEnumList(
    key: string,
    spec: ValueSpecListOf<'enum'>,
    current: string[],
  ) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        key,
        spec,
        current,
      },
      component: EnumListPage,
    })

    modal.onWillDismiss<string[]>().then(({ data }) => {
      if (!data) return
      this.updateEnumList(key, current, data)
    })

    await modal.present()
  }

  async presentAlertChangeWarning(
    key: string,
    spec: ValueSpec,
    okFn?: Function,
    cancelFn?: Function,
  ) {
    if (!spec.warning || this.warningAck[key]) return okFn ? okFn() : null
    this.warningAck[key] = true

    const buttons: AlertButton[] = [
      {
        text: 'Ok',
        handler: () => {
          if (okFn) okFn()
        },
        cssClass: 'enter-click',
      },
    ]

    if (okFn || cancelFn) {
      buttons.unshift({
        text: 'Cancel',
        handler: () => {
          if (cancelFn) cancelFn()
        },
      })
    }

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      subHeader: `Editing ${spec.name} has consequences:`,
      message: spec.warning,
      buttons,
    })
    await alert.present()
  }

  async presentAlertDelete(key: string, index: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Are you sure you want to delete this entry?',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.deleteListItem(key, index)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private deleteListItem(key: string, index: number, markDirty = true): void {
    if (this.objectListDisplay[key])
      this.objectListDisplay[key][index].height = '0px'
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    pauseFor(250).then(() => {
      if (this.objectListDisplay[key])
        this.objectListDisplay[key].splice(index, 1)
      arr.removeAt(index)
    })
  }

  private updateEnumList(key: string, current: string[], updated: string[]) {
    this.formGroup.get(key).markAsDirty()

    for (let i = current.length - 1; i >= 0; i--) {
      if (!updated.includes(current[i])) {
        this.deleteListItem(key, i, false)
      }
    }

    updated.forEach(val => {
      if (!current.includes(val)) {
        this.addListItem(key, false, val)
      }
    })
  }

  private getDocSize(key: string, index = 0) {
    const element = document.getElementById(this.getElementId(key, index))
    return `${element.scrollHeight}px`
  }

  getElementId(key: string, index = 0): string {
    return `${key}-${index}-${this.objectId}`
  }

  async presentUnionTagDescription(name: string, description: string) {
    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
    })
    await alert.present()
  }

  asIsOrder() {
    return 0
  }
}

interface HeaderData {
  spec: ValueSpec
  edited: boolean
  new: boolean
  invalid?: boolean
}

@Component({
  selector: 'form-label',
  templateUrl: './form-label.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormLabelComponent {
  Range = Range
  @Input() data: HeaderData

  constructor(private readonly alertCtrl: AlertController) {}

  async presentAlertDescription() {
    const { name, description } = this.data.spec

    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
    })
    await alert.present()
  }
}

@Component({
  selector: 'form-error',
  templateUrl: './form-error.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormErrorComponent {
  @Input() control: AbstractFormGroupDirective
  @Input() spec: ValueSpec
}
