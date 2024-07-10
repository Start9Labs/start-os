import { Component } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { BackupServerSelectModal } from 'src/app/modals/backup-server-select/backup-server-select.page'

@Component({
  selector: 'restore',
  templateUrl: './restore.component.html',
  styleUrls: ['./restore.component.scss'],
})
export class RestorePage {
  constructor(private readonly modalCtrl: ModalController) {}

  async presentModalSelectServer(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): Promise<void> {
    const modal = await this.modalCtrl.create({
      componentProps: { target },
      presentingElement: await this.modalCtrl.getTop(),
      component: BackupServerSelectModal,
    })

    await modal.present()
  }
}
