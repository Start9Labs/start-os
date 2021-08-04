import { Component, Input } from '@angular/core'
import { FormArray, FormGroup } from '@angular/forms'
import { AlertController, ModalController } from '@ionic/angular'
import { ConfigSpec, ListValueSpecOf, ValueSpec, ValueSpecList, ValueSpecListOf, ValueSpecUnion } from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'
import { Range } from 'src/app/pkg-config/config-utilities'
import { EnumListPage } from 'src/app/modals/enum-list/enum-list.page'

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
  Object = Object

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) { }

  getEnumListDisplay (arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec.valueNames[v]).join(', ')
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
    arr.insert(0, this.formService.getListItem(key, arr.length, this.objectSpec[key] as ValueSpecList, val))
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
    if (!spec.changeWarning || this.warningAck[key]) return
    this.warningAck[key] = true

    const alert = await this.alertCtrl.create({
      header: 'Warning',
      subHeader: `Editing ${spec.name} has consequences:`,
      message: spec.changeWarning,
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
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    arr.removeAt(index)
  }

  private updateEnumList (key: string, current: string[], updated: string[]) {
    this.formGroup.get(key).markAsDirty()

    let deleted = current.filter(x => !updated.includes(x))
    deleted.forEach((_, index) => this.deleteListItem(key, index, false))

    let added = updated.filter(x => !current.includes(x))
    added.forEach(val => this.addListItem(key, false, val))
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
