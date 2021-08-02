import { Component, EventEmitter, Input, Output } from '@angular/core'
import { FormArray, FormControl, FormGroup } from '@angular/forms'
import { AlertController } from '@ionic/angular'
import { ConfigSpec, ValueSpecList } from 'src/app/pkg-config/config-types'
import { FormService } from 'src/app/services/form.service'

@Component({
  selector: 'form-object',
  templateUrl: './form-object.component.html',
  styleUrls: ['./form-object.component.scss'],
})
export class FormObjectComponent {
  @Input() objectSpec: ConfigSpec
  @Input() formGroup: FormGroup
  @Output() onAdd = new EventEmitter<boolean>()

  constructor (
    private readonly alertCtrl: AlertController,
    private readonly formService: FormService,
  ) { }

  newListItem (key: string) {
    const arr = this.formGroup.get(key) as FormArray
    const validators = this.formService.getListItemValidators(this.objectSpec[key] as ValueSpecList, key, arr.length)
    arr.push(new FormControl('', validators))
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
            const arr = this.formGroup.get(key) as FormArray
            arr.removeAt(index)
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

