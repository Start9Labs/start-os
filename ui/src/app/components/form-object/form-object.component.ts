import { Component, Input, Output, SimpleChange, EventEmitter } from '@angular/core'
import { AbstractControl, AbstractFormGroupDirective, FormArray, FormGroup } from '@angular/forms'
import { AlertButton, AlertController, IonicSafeString, ModalController } from '@ionic/angular'
import { ConfigSpec, ListValueSpecOf, ListValueSpecString, ValueSpec, ValueSpecBoolean, ValueSpecEnum, ValueSpecList, ValueSpecListOf, ValueSpecNumber, ValueSpecString, ValueSpecUnion } from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'
import { Range } from 'src/app/pkg-config/config-utilities'
import { EnumListPage } from 'src/app/modals/enum-list/enum-list.page'
const Mustache = require('mustache')
import { pauseFor } from 'src/app/util/misc.util'

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
  warningAck: { [key: string]: boolean } = { }
  unmasked: { [key: string]: boolean } = { }
  // @TODO for when we want to expand/collapse normal objects/union in addition to list ones
  // objectExpanded: { [key: string]: boolean } = { }
  objectListInfo: { [key: string]: { expanded: boolean, height: string, displayAs: string }[] } = { }
  Object = Object

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) { }

  ngOnChanges (changes: { [propName: string]: SimpleChange }) {
    // Lists are automatically expanded, but their members are not
    Object.keys(this.objectSpec).forEach(key => {
      const spec = this.objectSpec[key]
      if (spec.type === 'list' && ['object', 'union'].includes(spec.subtype)) {
        this.objectListInfo[key] = [];
        (this.formGroup.get(key).value as any[]).forEach((obj, index) => {
          const displayAs = (spec.spec as ListValueSpecOf<'object'>)['display-as']
          this.objectListInfo[key][index] = {
            expanded: false,
            height: '0px',
            displayAs: displayAs ? (Mustache as any).render(displayAs, obj) : '',
          }
        })
      }
    })
  }

  getEnumListDisplay (arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec['value-names'][v]).join(', ')
  }

  updateUnion (e: any): void {
    Object.keys(this.formGroup.controls).forEach(control => {
      if (control === 'type') return
      this.formGroup.removeControl(control)
    })

    const unionGroup = this.formService.getUnionObject(this.unionSpec as ValueSpecUnion, e.detail.value)

    Object.keys(unionGroup.controls).forEach(control => {
      if (control === 'type') return
      this.formGroup.addControl(control, unionGroup.controls[control])
    })
  }

  addListItemWrapper (key: string, spec: ValueSpec) {
    this.presentAlertChangeWarning(key, spec, () => this.addListItem(key))
  }

  addListItem (key: string, markDirty = true, val?: string): void {
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    // const validators = this.formService.getListItemValidators(this.objectSpec[key] as ValueSpecList, key, arr.length)
    // arr.push(new FormControl(value, validators))
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(listSpec, val)
    newItem.markAllAsTouched()
    arr.insert(0, newItem)
    if (['object', 'union'].includes(listSpec.subtype)) {
      const displayAs = (listSpec.spec as ListValueSpecOf<'object'>)['display-as']
      this.objectListInfo[key].unshift({
        height: '0px',
        expanded: true,
        displayAs: displayAs ? Mustache.render(displayAs, newItem.value) : '',
      })

      pauseFor(200).then(() => {
        this.objectListInfo[key][0].height = this.getDocSize(key)
      })
    }
  }

  toggleExpand (key: string, i: number) {
    this.objectListInfo[key][i].expanded = !this.objectListInfo[key][i].expanded
    this.objectListInfo[key][i].height = this.objectListInfo[key][i].expanded ? this.getDocSize(key) : '0px'
  }

  updateLabel (key: string, i: number, displayAs: string) {
    this.objectListInfo[key][i].displayAs = displayAs ? Mustache.render(displayAs, this.formGroup.get(key).value[i]) : ''
  }

  getWarningText (text: string): IonicSafeString {
    if (text) return new IonicSafeString(`<ion-text color="warning">${text}</ion-text>`)
  }

  handleInputChange () {
    this.onInputChange.emit()
  }

  handleBooleanChange (key: string, spec: ValueSpecBoolean) {
    if (spec.warning) {
      const current = this.formGroup.get(key).value
      const cancelFn = () => this.formGroup.get(key).setValue(!current)
      this.presentAlertChangeWarning(key, spec, undefined, cancelFn)
    }
  }

  async presentModalEnumList (key: string, spec: ValueSpecListOf<'enum'>, current: string[]) {
    const modal = await this.modalCtrl.create({
      componentProps: {
        key,
        spec,
        current,
      },
      component: EnumListPage,
    })

    modal.onWillDismiss().then(res => {
      const data = res.data
      if (!data) return
      this.updateEnumList(key, current, data)
    })

    await modal.present()
  }

  async presentAlertChangeWarning (key: string, spec: ValueSpec, okFn?: Function, cancelFn?: Function) {
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

  async presentAlertDelete (key: string, index: number) {
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

  private deleteListItem (key: string, index: number, markDirty = true): void {
    if (this.objectListInfo[key]) this.objectListInfo[key][index].height = '0px'
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    pauseFor(500).then(() => {
      if (this.objectListInfo[key]) this.objectListInfo[key].splice(index, 1)
      arr.removeAt(index)
    })
  }

  private updateEnumList (key: string, current: string[], updated: string[]) {
    this.formGroup.get(key).markAsDirty()

    let deleted = current.filter(x => !updated.includes(x))
    deleted.forEach((_, index) => this.deleteListItem(key, index, false))

    let added = updated.filter(x => !current.includes(x))
    added.forEach(val => this.addListItem(key, false, val))
  }

   getDocSize (selected: string) {
    const element = document.getElementById(selected)
    return `${element.scrollHeight}px`
  }

  asIsOrder () {
    return 0
  }
}

interface HeaderData {
  spec: ValueSpec
  isEdited: boolean
  isNew: boolean
}

@Component({
  selector: 'form-label',
  templateUrl: './form-label.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormLabelComponent {
  Range = Range
  @Input() data: HeaderData

  constructor (
    private readonly alertCtrl: AlertController,
  ) { }

  async presentAlertDescription () {
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
