import { Component } from '@angular/core'
import {
  BackupTarget,
  BackupTargetType,
  RR,
  UnknownDisk,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  cifsSpec,
  diskBackupTargetSpec,
  dropboxSpec,
  googleDriveSpec,
  remoteBackupTargetSpec,
} from '../../types/target-types'
import { BehaviorSubject, filter } from 'rxjs'
import { TuiDialogService } from '@taiga-ui/core'
import { ErrorService } from '@start9labs/shared'
import { FormDialogService } from '../../../../services/form-dialog.service'
import { FormPage } from '../../../../modals/form/form.page'
import { LoadingService } from '../../../../modals/loading/loading.service'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import {
  InputSpec,
  unionSelectKey,
  unionValueKey,
} from '@start9labs/start-sdk/lib/config/configTypes'

type BackupConfig =
  | {
      type: {
        [unionSelectKey]: 'dropbox' | 'google-drive'
        [unionValueKey]: RR.AddCloudBackupTargetReq
      }
    }
  | {
      type: {
        [unionSelectKey]: 'cifs'
        [unionValueKey]: RR.AddCifsBackupTargetReq
      }
    }

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
    private readonly dialogs: TuiDialogService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
    private readonly formDialog: FormDialogService,
  ) {}

  ngOnInit() {
    this.getTargets()
  }

  async presentModalAddPhysical(disk: UnknownDisk, index: number) {
    this.formDialog.open(FormPage, {
      label: 'New Physical Target',
      data: {
        spec: await configBuilderToSpec(diskBackupTargetSpec),
        value: {
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

                return true
              }),
          },
        ],
      },
    })
  }

  async presentModalAddRemote() {
    this.formDialog.open(FormPage, {
      label: 'New Remote Target',
      data: {
        spec: await configBuilderToSpec(remoteBackupTargetSpec),
        buttons: [
          {
            text: 'Save',
            handler: ({ type }: BackupConfig) =>
              this.add(
                type[unionSelectKey] === 'cifs' ? 'cifs' : 'cloud',
                type[unionValueKey],
              ),
          },
        ],
      },
    })
  }

  async presentModalUpdate(target: BackupTarget) {
    let spec: InputSpec

    switch (target.type) {
      case 'cifs':
        spec = await configBuilderToSpec(cifsSpec)
        break
      case 'cloud':
        spec = await configBuilderToSpec(
          target.provider === 'dropbox' ? dropboxSpec : googleDriveSpec,
        )
        break
      case 'disk':
        spec = await configBuilderToSpec(diskBackupTargetSpec)
        break
    }

    this.formDialog.open(FormPage, {
      label: 'Update Target',
      data: {
        spec,
        value: target,
        buttons: [
          {
            text: 'Save',
            handler: (
              value:
                | RR.UpdateCifsBackupTargetReq
                | RR.UpdateCloudBackupTargetReq
                | RR.UpdateDiskBackupTargetReq,
            ) => this.update(target.type, { ...value, id: target.id }),
          },
        ],
      },
    })
  }

  presentAlertDelete(id: string, index: number) {
    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Forget backup target? This actions cannot be undone.',
          no: 'Cancel',
          yes: 'Delete',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.delete(id, index))
  }

  async delete(id: string, index: number): Promise<void> {
    const loader = this.loader.open('Removing...').subscribe()

    try {
      await this.api.removeBackupTarget({ id })
      this.targets.saved.splice(index, 1)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
      this.errorService.handleError(e)
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
    const loader = this.loader.open('Saving target...').subscribe()

    try {
      return await this.api.addBackupTarget(type, value)
    } finally {
      loader.unsubscribe()
    }
  }

  private async update(
    type: BackupTargetType,
    value:
      | RR.UpdateCifsBackupTargetReq
      | RR.UpdateCloudBackupTargetReq
      | RR.UpdateDiskBackupTargetReq,
  ): Promise<BackupTarget> {
    const loader = this.loader.open('Saving target...').subscribe()

    try {
      return await this.api.updateBackupTarget(type, value)
    } finally {
      loader.unsubscribe()
    }
  }
}
