import { Component, Input, SimpleChange } from '@angular/core'
import { FormArray, FormGroup } from '@angular/forms'
import { AlertController, ModalController } from '@ionic/angular'
import { ConfigSpec, ListValueSpecOf, ValueSpec, ValueSpecList, ValueSpecListOf, ValueSpecUnion } from 'src/app/pkg-config/config-types'
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
    // @TODO figure out why changes are being triggered so often. If too heavy, switch to ngOnInit and figure out another way to manually reset defaults is executed. Needed because otherwise ObjectListInfo won't be accurate.

    // if ( changes['current'] && changes['current'].previousValue != changes['current'].currentValue ) {
    //   console.log('CURRENT')
    // }
    // if ( changes['formGroup'] && changes['formGroup'].previousValue != changes['formGroup'].currentValue ) {
    //   console.log('FORM GROUP')
    // }
    // if ( changes['objectSpec'] && changes['objectSpec'].previousValue != changes['objectSpec'].currentValue ) {
    //   console.log('OBJECT SPEC')
    // }
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

  addListItem (key: string, markDirty = true, val?: string): void {
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    // const validators = this.formService.getListItemValidators(this.objectSpec[key] as ValueSpecList, key, arr.length)
    // arr.push(new FormControl(value, validators))
    const listSpec = this.objectSpec[key] as ValueSpecList
    const newItem = this.formService.getListItem(key, arr.length, listSpec, val)
    newItem.markAllAsTouched()
    arr.insert(0, newItem)
    if (['object', 'union'].includes(listSpec.subtype)) {
      const displayAs = (listSpec.spec as ListValueSpecOf<'object'>)['display-as']
      this.objectListInfo[key].push({
        height: '0px',
        expanded: true,
        displayAs: displayAs ? Mustache.render(displayAs, newItem.value) : '',
      })

      pauseFor(200).then(() => {
        const index = this.objectListInfo[key].length - 1
        this.objectListInfo[key][index].height = this.getDocSize(key)
      })
    }
  }

  toggleExpand (key: string, i: number) {
    this.objectListInfo[key][i].expanded = !this.objectListInfo[key][i].expanded
    this.objectListInfo[key][i].height = this.objectListInfo[key][i].expanded ? this.getDocSize(key) : '0px'

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

  async presentAlertChangeWarning (key: string, spec: ValueSpec) {
    if (!spec['change-warning'] || this.warningAck[key]) return
    this.warningAck[key] = true

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      subHeader: `Editing ${spec.name} has consequences:`,
      message: spec['change-warning'],
      buttons: ['Ok'],
    })
    await alert.present()
  }

  async presentAlertDelete (key: string, index: number) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
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
        },
      ],
    })
    await alert.present()
  }

  private deleteListItem (key: string, index: number, markDirty = true): void {
    this.objectListInfo[key][index].height = '0px'
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    pauseFor(500).then(() => {
      this.objectListInfo[key].splice(index, 1)
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
      buttons: ['Ok'],
    })
    await alert.present()
  }
}
