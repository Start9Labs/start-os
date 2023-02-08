import { Component } from '@angular/core'
import {
  BackupTarget,
  BackupTargetType,
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

export type BackupType = 'create' | 'restore'

@Component({
  selector: 'backup-targets',
  templateUrl: './backup-targets.page.html',
  styleUrls: ['./backup-targets.page.scss'],
})
export class BackupTargetsPage {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-targets'
  targets: {
    'unsaved-physical': DiskBackupTarget[]
    saved: BackupTarget[]
  } = {
    'unsaved-physical': [],
    saved: [],
  }

  loading$ = new BehaviorSubject(true)
  error$ = new Subject<string>()

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly alertCtrl: AlertController,
    private readonly loadingCtrl: LoadingController,
    private readonly errToast: ErrorToastService,
    private readonly api: ApiService,
  ) {}

  ngOnInit() {
    this.getTargets()
  }

  async presentModalAdd(): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'New Remote Target',
        spec: RemoteBackupTargetSpec,
        buttons: [
          {
            text: 'Save',
            handler: (
              value:
                | (RR.AddCifsBackupTargetReq & { type: BackupTargetType })
                | (RR.AddCloudBackupTargetReq & { type: BackupTargetType }),
            ) => {
              return this.saveTarget(value.type, value)
            },
            isSubmit: true,
          },
        ],
      },
    })

    await modal.present()
  }

  async presentModalUpdate(target: BackupTarget): Promise<void> {
    let spec: typeof RemoteBackupTargetSpec = {}

    switch (target.type) {
      case 'cifs':
        spec = CifsSpec
        break
      case 'cloud':
        spec = target.provider === 'dropbox' ? DropboxSpec : GoogleDriveSpec
        break
    }

    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Update Remote Target',
        spec,
        buttons: [
          {
            text: 'Save',
            handler: (
              value:
                | RR.UpdateCifsBackupTargetReq
                | RR.UpdateCloudBackupTargetReq,
            ) => {
              return this.saveTarget(target.type, value)
            },
            isSubmit: true,
          },
        ],
        initialValue: target,
      },
    })
    await modal.present()
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

  async refresh() {
    this.loading$.next(true)
    this.error$.next('')
    await this.getTargets()
  }

  private async getTargets(): Promise<void> {
    try {
      const targets = await this.api.getBackupTargets({})
      this.targets = {
        'unsaved-physical': [],
        saved: targets,
      }
    } catch (e: any) {
      this.error$.next(e.message)
    } finally {
      this.loading$.next(false)
    }
  }

  private async saveTarget(
    type: BackupTargetType,
    value: RR.AddCifsBackupTargetReq | RR.AddCloudBackupTargetReq,
  ): Promise<any> {
    const loader = await this.loadingCtrl.create({
      message: 'Saving target...',
    })
    await loader.present()

    try {
      const res = await this.api.addBackupTarget(type, value)
      return res
    } finally {
      loader.dismiss()
    }
  }
}
