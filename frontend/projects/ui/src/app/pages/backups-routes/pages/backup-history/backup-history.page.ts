import { Component } from '@angular/core'
import {
  BackupTarget,
  DiskBackupTarget,
  RR,
} from 'src/app/services/api/api.types'
import {
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import {
  CifsSpec,
  DropboxSpec,
  GoogleDriveSpec,
  RemoteBackupTargetSpec,
} from '../../types/target-types'
import { BehaviorSubject, Subject } from 'rxjs'

@Component({
  selector: 'backup-history',
  templateUrl: './backup-history.page.html',
  styleUrls: ['./backup-history.page.scss'],
})
export class BackupHistoryPage {
  constructor(
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  ngOnInit() {
    this.getHistory()
  }

  async presentAlertDelete(id: string, index: number) {
    const alert = await this.alertCtrl.create({
      header: 'Confirm',
      message: 'Forget backup target? This actions cannot be undone.',
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: 'Delete',
          handler: () => {
            this.delete(id, index)
          },
          cssClass: 'enter-click',
        },
      ],
    })
    await alert.present()
  }

  async delete(id: string, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Removing...',
    })
    await loader.present()

    try {
      await this.api.removeBackupTarget({ id })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async getHistory(): Promise<void> {
    try {
      const runs = await this.api.getBackupTargets({})
    } catch (e: any) {
      this.errToast.present(e)
    }
  }
}
