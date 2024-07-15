import { TuiButton, TuiNotification } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { Component, inject, OnInit, signal } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { CT } from '@start9labs/start-sdk'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import {
  BackupTarget,
  BackupTargetType,
  RR,
  UnknownDisk,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { BackupsPhysicalComponent } from '../components/physical.component'
import { BackupsTargetsComponent } from '../components/targets.component'
import { BackupConfig } from '../types/backup-config'
import {
  cifsSpec,
  diskBackupTargetSpec,
  dropboxSpec,
  googleDriveSpec,
  remoteBackupTargetSpec,
} from '../types/target'

@Component({
  template: `
    <tui-notification>
      Backup targets are physical or virtual locations for storing encrypted
      backups. They can be physical drives plugged into your server, shared
      folders on your Local Area Network (LAN), or third party clouds such as
      Dropbox or Google Drive.
      <a
        href="https://docs.start9.com/latest/user-manual/backups/backup-targets"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
    <h3 class="g-title">
      Unknown Physical Drives
      <button
        tuiButton
        size="s"
        iconStart="@tui.refresh-cw"
        (click)="refresh()"
      >
        Refresh
      </button>
    </h3>
    <table
      class="g-table"
      [backupsPhysical]="targets()?.unknownDisks || null"
      (add)="addPhysical($event)"
    ></table>
    <h3 class="g-title">
      Saved Targets
      <button tuiButton size="s" iconStart="@tui.plus" (click)="addRemote()">
        Add Target
      </button>
    </h3>
    <table
      class="g-table"
      [backupsTargets]="targets()?.saved || null"
      (delete)="onDelete($event)"
      (update)="onUpdate($event)"
    ></table>
  `,
  standalone: true,
  imports: [
    CommonModule,
    TuiNotification,
    TuiButton,
    BackupsPhysicalComponent,
    BackupsTargetsComponent,
  ],
})
export class BackupsTargetsModal implements OnInit {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)

  targets = signal<RR.GetBackupTargetsRes | null>(null)

  ngOnInit() {
    this.refresh()
  }

  async refresh() {
    this.targets.set(null)

    try {
      this.targets.set(await this.api.getBackupTargets({}))
    } catch (e: any) {
      this.errorService.handleError(e)
      this.targets.set({ unknownDisks: [], saved: [] })
    }
  }

  async onDelete(id: string) {
    const loader = this.loader.open('Removing...').subscribe()

    try {
      await this.api.removeBackupTarget({ id })
      this.setTargets(this.targets()?.saved.filter(a => a.id !== id))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async onUpdate(value: BackupTarget) {
    this.formDialog.open(FormComponent, {
      label: 'Update Target',
      data: {
        value,
        spec: await this.getSpec(value),
        buttons: [
          {
            text: 'Save',
            handler: (
              response:
                | RR.UpdateCifsBackupTargetReq
                | RR.UpdateCloudBackupTargetReq
                | RR.UpdateDiskBackupTargetReq,
            ) => this.update(value.type, { ...response, id: value.id }),
          },
        ],
      },
    })
  }

  async addPhysical(disk: UnknownDisk) {
    this.formDialog.open(FormComponent, {
      label: 'New Physical Target',
      data: {
        spec: await configBuilderToSpec(diskBackupTargetSpec),
        value: { name: disk.label || disk.logicalname },
        buttons: [
          {
            text: 'Save',
            handler: (value: Omit<RR.AddDiskBackupTargetReq, 'logicalname'>) =>
              this.add('disk', {
                logicalname: disk.logicalname,
                ...value,
              }).then(response => {
                this.setTargets(
                  this.targets()?.saved.concat(response),
                  this.targets()?.unknownDisks.filter(a => a !== disk),
                )
                return true
              }),
          },
        ],
      },
    })
  }

  async addRemote() {
    this.formDialog.open(FormComponent, {
      label: 'New Remote Target',
      data: {
        spec: await configBuilderToSpec(remoteBackupTargetSpec),
        buttons: [
          {
            text: 'Save',
            handler: ({ type }: BackupConfig) =>
              this.add(
                type[CT.unionSelectKey] === 'cifs' ? 'cifs' : 'cloud',
                type[CT.unionValueKey],
              ),
          },
        ],
      },
    })
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

  private setTargets(
    saved: BackupTarget[] = this.targets()?.saved || [],
    unknownDisks: UnknownDisk[] = this.targets()?.unknownDisks || [],
  ) {
    this.targets.set({ unknownDisks, saved })
  }

  private async getSpec(target: BackupTarget) {
    switch (target.type) {
      case 'cifs':
        return await configBuilderToSpec(cifsSpec)
      case 'cloud':
        return await configBuilderToSpec(
          target.provider === 'dropbox' ? dropboxSpec : googleDriveSpec,
        )
      case 'disk':
        return await configBuilderToSpec(diskBackupTargetSpec)
    }
  }
}

export const TARGETS = new PolymorpheusComponent(BackupsTargetsModal)
