import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '../../../types'
import { ModalController } from '@ionic/angular'
import { ReleaseNotesComponent } from '../../../modals/release-notes/release-notes.component'

@Component({
  selector: 'marketplace-about',
  templateUrl: 'about.component.html',
  styleUrls: ['about.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AboutComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  constructor(private readonly modalCtrl: ModalController) {}

  async presentModalNotes() {
    const modal = await this.modalCtrl.create({
      componentProps: { pkg: this.pkg },
      component: ReleaseNotesComponent,
    })

    await modal.present()
  }
}
