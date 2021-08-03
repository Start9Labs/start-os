import { Component, Input } from '@angular/core'
import { FormArray, FormControl, FormGroup } from '@angular/forms'
import { AlertController } from '@ionic/angular'
import { ConfigSpec, ListValueSpecOf, ValueSpecList, ValueSpecUnion } from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec: ConfigSpec
  @Input() formGroup: FormGroup
  @Input() unionSpec: ValueSpecUnion
  Object = Object
  console = console

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly formService: FormService,
  ) { }

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

  updateEnumList (key: string, current: string[], e: any) {
    this.formGroup.get(key).markAsDirty()

    const updated = e.detail.value as string[]

    let deleted = current.filter(x => !updated.includes(x))
    deleted.forEach((_, index) => this.deleteListItem(key, index, false))

    let added = updated.filter(x => !current.includes(x))
    added.forEach(val => this.addListItem(key, val, false))
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

