import { Component } from '@angular/core'
import {
  BackupTarget,
  RemoteBackupTarget,
  RR,
} from 'src/app/services/api/api.types'
import { LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { Subject } from 'rxjs'
import {
  CifsSpec,
  DropboxSpec,
  GoogleDriveSpec,
  RemoteBackupTargetSpec,
  TargetService,
} from '../../services/target-service'

export type BackupType = 'create' | 'restore'
export type WithId<T> = T & { id: string }

@Component({
  selector: 'backup-targets',
  templateUrl: './backup-targets.page.html',
  styleUrls: ['./backup-targets.page.scss'],
})
export class BackupTargetsPage {
  readonly targets$ = new Subject<WithId<BackupTarget>[]>()

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly targetService: TargetService,
  ) {}

  async ngOnInit() {
    const targets = await this.getBackupTargets()
    this.targets$.next(targets)
  }

  private async getBackupTargets(): Promise<WithId<BackupTarget>[]> {
    const targets = await this.api.getBackupTargets({})
    return Object.keys(targets).map(id => ({ id, ...targets[id] }))
  }

  async presentModalEditRemoteTarget(
    target: WithId<RemoteBackupTarget>,
  ): Promise<void> {
    let spec: typeof RemoteBackupTargetSpec = {}
    let initialValue: any = null

    switch (target.type) {
      case 'cifs':
        spec = CifsSpec
        initialValue = {
          hostname: target.hostname,
          path: target.path,
          username: target.username,
        }
        break
      case 'cloud':
        spec = target.provider === 'dropbox' ? DropboxSpec : GoogleDriveSpec
        initialValue = {
          path: target.path,
        }
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
            handler: (value: RR.AddBackupTargetReq) => {
              return this.targetService.saveTarget({ id: target.id, ...value })
            },
            isSubmit: true,
          },
        ],
        initialValue,
      },
    })
    await modal.present()
  }

  async deleteTarget(id: string, index: number): Promise<void> {
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
}
