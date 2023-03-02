import { Component } from '@angular/core'
import {
  BackupTarget,
  BackupTargetType,
  DiskBackupTarget,
  RR,
  UnknownDisk,
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
  DiskBackupTargetSpec,
  RemoteBackupTargetSpec,
} from '../../types/target-types'
import { BehaviorSubject } from 'rxjs'

export type BackupType = 'create' | 'restore'

@Component({
  selector: 'backup-targets',
  templateUrl: './backup-targets.page.html',
  styleUrls: ['./backup-targets.page.scss'],
})
export class BackupTargetsPage {
  readonly docsUrl =
    'https://docs.start9.com/latest/user-manual/backups/backup-targets'
  targets: RR.GetBackupTargetsRes = {
    'unknown-disks': [],
    saved: [],
  }

  loading$ = new BehaviorSubject(true)

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

  async presentModalAddPhysical(
    disk: UnknownDisk,
    index: number,
  ): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'New Physical Target',
        spec: DiskBackupTargetSpec,
        initialValue: {
          name: disk.label || disk.logicalname,
        },
        buttons: [
          {
            text: 'Save',
            handler: (value: Omit<RR.AddDiskBackupTargetReq, 'logicalname'>) =>
              this.add('disk', {
                logicalname: disk.logicalname,
                ...value,
              }).then(disk => {
                this.targets['unknown-disks'].splice(index, 1)
                this.targets.saved.push(disk)
              }),
            isSubmit: true,
          },
        ],
      },
    })

    await modal.present()
  }

  async presentModalAddRemote(): Promise<void> {
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
            ) => this.add(value.type, value),
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
      case 'disk':
        spec = DiskBackupTargetSpec
        break
    }

    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Update Remote Target',
        spec,
        initialValue: target,
        buttons: [
          {
            text: 'Save',
            handler: (
              value:
                | RR.UpdateCifsBackupTargetReq
                | RR.UpdateCloudBackupTargetReq
                | RR.UpdateDiskBackupTargetReq,
            ) => this.update(target.type, value),
            isSubmit: true,
          },
        ],
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
      this.targets.saved.splice(index, 1)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  async refresh() {
    this.loading$.next(true)
    await this.getTargets()
  }

  getIcon(type: BackupTargetType) {
    switch (type) {
      case 'disk':
        return 'save-outline'
      case 'cifs':
        return 'folder-open-outline'
      case 'cloud':
        return 'cloud-outline'
    }
  }

  private async getTargets(): Promise<void> {
    try {
      this.targets = await this.api.getBackupTargets({})
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      this.loading$.next(false)
    }
  }

  private async add(
    type: BackupTargetType,
    value:
      | RR.AddCifsBackupTargetReq
      | RR.AddCloudBackupTargetReq
      | RR.AddDiskBackupTargetReq,
  ): Promise<BackupTarget> {
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

  private async update(
    type: BackupTargetType,
    value:
      | RR.UpdateCifsBackupTargetReq
      | RR.UpdateCloudBackupTargetReq
      | RR.UpdateDiskBackupTargetReq,
  ): Promise<BackupTarget> {
    const loader = await this.loadingCtrl.create({
      message: 'Saving target...',
    })
    await loader.present()

    try {
      const res = await this.api.updateBackupTarget(type, value)
      return res
    } finally {
      loader.dismiss()
    }
  }
}
