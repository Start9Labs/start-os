import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { StartOSDiskInfoWithId } from 'src/app/services/api/api.service'
import { PasswordPage } from '../password/password.page'

@Component({
  selector: 'server-backup-select',
  templateUrl: 'server-backup-select.page.html',
  styleUrls: ['server-backup-select.page.scss'],
})
export class ServerBackupSelectModal {
  @Input() servers: StartOSDiskInfoWithId[] = []

  constructor(private readonly modalController: ModalController) {}

  cancel() {
    this.modalController.dismiss()
  }

  async select(server: StartOSDiskInfoWithId): Promise<void> {
    this.presentModalPassword(server)
  }

  private async presentModalPassword(
    server: StartOSDiskInfoWithId,
  ): Promise<void> {
    const modal = await this.modalController.create({
      component: PasswordPage,
      componentProps: { passwordHash: server.passwordHash },
    })
    modal.onDidDismiss().then(res => {
      if (res.role === 'success') {
        this.modalController.dismiss(
          {
            serverId: server.id,
            recoveryPassword: res.data.password,
          },
          'success',
        )
      }
    })
    await modal.present()
  }
}
