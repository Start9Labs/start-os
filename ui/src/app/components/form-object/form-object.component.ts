import { Component, Input } from '@angular/core'
import { FormArray, FormControl, FormGroup } from '@angular/forms'
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
  @Input() original?: { [key: string]: any } = { }
  Object = Object

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly formService: FormService,
  ) { }

  ngOnInit () {
    console.log('ObjSpec', this.objectSpec)
    console.log('form Group', this.formGroup)
  }

  getEnumListDisplay (arr: string[], spec: ListValueSpecOf<'enum'>): string {
    return arr.map((v: string) => spec.valueNames[v]).join(',')
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

  addListItem (key: string, value = '', markDirty = true): void {
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    const validators = this.formService.getListItemValidators(this.objectSpec[key] as ValueSpecList, key, arr.length)
    arr.push(new FormControl(value, validators))
  }

  deleteListItem (key: string, index: number, markDirty = true): void {
    const arr = this.formGroup.get(key) as FormArray
    if (markDirty) arr.markAsDirty()
    arr.removeAt(index)
  }

  updateEnumList (key: string, current: string[], updated: string[]) {
    this.formGroup.get(key).markAsDirty()

    let deleted = current.filter(x => !updated.includes(x))
    deleted.forEach((_, index) => this.deleteListItem(key, index, false))

    let added = updated.filter(x => !current.includes(x))
    added.forEach(val => this.addListItem(key, val, false))
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

  async presentAlertDelete (key: string, index: number) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: 'Caution',
      message: `Are you sure you want to delete this entry?`,
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

  asIsOrder () {
    return 0
  }
}

interface HeaderData {
  key: string
  original: any
  spec: ValueSpec
}

@Component({
  selector: 'form-object-header',
  templateUrl: './form-object-header.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectHeaderComponent {
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
