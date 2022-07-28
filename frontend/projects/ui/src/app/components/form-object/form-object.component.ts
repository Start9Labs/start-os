import { Component, Input, Output, EventEmitter } from '@angular/core'
import {
  AbstractFormGroupDirective,
  UntypedFormArray,
  UntypedFormGroup,
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

interface Config {
  [key: string]: any
}
@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec!: ConfigSpec
  @Input() formGroup!: UntypedFormGroup
  @Input() unionSpec?: ValueSpecUnion
  @Input() current?: Config
  @Input() original?: Config
  @Output() onInputChange = new EventEmitter<void>()
  @Output() onExpand = new EventEmitter<void>()
  @Output() hasNewOptions = new EventEmitter<void>()
  warningAck: { [key: string]: boolean } = {}
  unmasked: { [key: string]: boolean } = {}
  objectDisplay: {
    [key: string]: { expanded: boolean; height: string; hasNewOptions: boolean }
  } = {}
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
        this.formGroup.get(key)?.value.forEach((obj: any, index: number) => {
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
          hasNewOptions: false,
        }
      }
    })

    // setTimeout hack to avoid ExpressionChangedAfterItHasBeenCheckedError
    setTimeout(() => {
      if (this.original) {
        Object.keys(this.current || {}).forEach(key => {
          if ((this.original as Config)[key] === undefined) {
            this.hasNewOptions.emit()
          }
        })
      }
    }, 10)
  }

  getEnumListDisplay(arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec['value-names'][v]).join(', ')
  }

  updateUnion(e: any): void {
    const primary = this.unionSpec?.tag.id

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

    Object.entries(this.unionSpec?.variants[e.detail.value] || {}).forEach(
      ([key, value]) => {
        if (['object', 'union'].includes(value.type)) {
          this.objectDisplay[key] = {
            expanded: false,
            height: '0px',
            hasNewOptions: false,
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
    const arr = this.formGroup.get(key) as UntypedFormArray
    if (markDirty) arr.markAsDirty()
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(listSpec, val)

    if (!newItem) return

    const index = arr.length

    newItem.markAllAsTouched()
    arr.insert(index, newItem)
    if (['object', 'union'].includes(listSpec.subtype)) {
      const displayAs = (listSpec.spec as ListValueSpecOf<'object'>)[
        'display-as'
      ]
      this.objectListDisplay[key].push({
        height: '0px',
        expanded: false,
        displayAs: displayAs ? Mustache.render(displayAs, newItem.value) : '',
      })
    }

    pauseFor(400).then(() => {
      const element = document.getElementById(this.getElementId(key, index))
      element?.parentElement?.scrollIntoView({ behavior: 'smooth' })
    })
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
      ? Mustache.render(displayAs, this.formGroup.get(key)?.value[i])
      : ''
  }

  getWarningText(text: string = ''): IonicSafeString | string {
    return text
      ? new IonicSafeString(`<ion-text color="warning">${text}</ion-text>`)
      : ''
  }

  handleInputChange() {
    this.onInputChange.emit()
  }

  setHasNew(key: string) {
    this.hasNewOptions.emit()
    setTimeout(() => {
      this.objectDisplay[key].hasNewOptions = true
    }, 100)
  }

  handleBooleanChange(key: string, spec: ValueSpecBoolean) {
    if (spec.warning) {
      const current = this.formGroup.get(key)?.value
      const cancelFn = () => this.formGroup.get(key)?.setValue(!current)
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

  async presentAlertDescription(event: Event, spec: ValueSpec) {
    event.stopPropagation()
    const { name, description } = spec

    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
      buttons: [
        {
          text: 'OK',
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  private deleteListItem(key: string, index: number, markDirty = true): void {
    if (this.objectListDisplay[key])
      this.objectListDisplay[key][index].height = '0px'
    const arr = this.formGroup.get(key) as UntypedFormArray
    if (markDirty) arr.markAsDirty()
    pauseFor(250).then(() => {
      if (this.objectListDisplay[key])
        this.objectListDisplay[key].splice(index, 1)
      arr.removeAt(index)
    })
  }

  private updateEnumList(key: string, current: string[], updated: string[]) {
    this.formGroup.get(key)?.markAsDirty()

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

  private getDocSize(key: string, index = 0): string {
    const element = document.getElementById(this.getElementId(key, index))
    return `${element?.scrollHeight}px`
  }

  getElementId(key: string, index = 0): string {
    return `${key}-${index}-${this.objectId}`
  }

  async presentUnionTagDescription(
    event: Event,
    name: string,
    description: string,
  ) {
    event.stopPropagation()
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
  newOptions?: boolean
}

@Component({
  selector: 'form-label',
  templateUrl: './form-label.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormLabelComponent {
  Range = Range
  @Input() data!: HeaderData

  constructor(private readonly alertCtrl: AlertController) {}

  async presentAlertDescription(event: Event) {
    event.stopPropagation()
    const { name, description } = this.data.spec

    const alert = await this.alertCtrl.create({
      header: name,
      message: description,
      buttons: [
        {
          text: 'OK',
          cssClass: 'enter-click',
        },
      ],
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
  @Input() control!: AbstractFormGroupDirective
  @Input() spec!: ValueSpec
}
