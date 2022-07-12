import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'

@Component({
  selector: 'action-marketplace',
  templateUrl: './action-marketplace.component.html',
  styleUrls: ['./action-marketplace.component.scss'],
})
export class ActionMarketplaceComponent {
  @Input() title!: string
  @Input() packageMarketplace!: string
  @Input() currentMarketplace!: string
  @Input() pkgId!: string

  constructor(private readonly modalCtrl: ModalController) {}

  dismiss() {
    this.modalCtrl.dismiss()
  }
}
