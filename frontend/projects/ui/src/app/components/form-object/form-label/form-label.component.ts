import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { AlertController } from '@ionic/angular'

@Component({
  selector: 'form-label',
  templateUrl: './form-label.component.html',
  styleUrls: ['./form-label.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FormLabelComponent {
  @Input() data!: {
    name: string
    description: string | null
    edited?: boolean
    new?: boolean
    required?: boolean
    newOptions?: boolean
  }

  constructor(private readonly alertCtrl: AlertController) {}

  async presentAlertDescription() {
    const alert = await this.alertCtrl.create({
      header: this.data.name,
      message: this.data.description || '',
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
