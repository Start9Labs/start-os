import { CommonModule } from '@angular/common'
import { Component, inject, OnInit } from '@angular/core'
import { ErrorService, LoadingService } from '@start9labs/shared'
import {
  unionSelectKey,
  unionValueKey,
} from '@start9labs/start-sdk/lib/config/configTypes'
import { TuiNotificationModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiFadeModule } from '@taiga-ui/experimental'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import {
  cifsSpec,
  diskBackupTargetSpec,
  dropboxSpec,
  googleDriveSpec,
  remoteBackupTargetSpec,
} from 'src/app/apps/ui/pages/backups/types/target-types'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import {
  BackupTarget,
  BackupTargetType,
  RR,
  UnknownDisk,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupConfig } from '../types/backup-config'
import { BackupsPhysicalComponent } from '../components/physical.component'
import { BackupsTargetsComponent } from '../components/targets.component'

@Component({
  template: `
    <ng-container *ngIf="loading$ | async"></ng-container>
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
      <button tuiButton size="s" icon="tuiIconRefreshCw" (click)="refresh()">
        Refresh
      </button>
    </h3>
    <div class="g-hidden-scrollbar" tuiFade>
      <table
        class="g-table"
        [backupsPhysical]="targets?.['unknown-disks'] || null"
        (add)="addPhysical($event)"
      ></table>
    </div>
    <h3 class="g-title">
      Saved Targets
      <button tuiButton size="s" icon="tuiIconPlus" (click)="addRemote()">
        Add Target
      </button>
    </h3>
    <div class="g-hidden-scrollbar" tuiFade>
      <table
        class="g-table"
        [backupsTargets]="targets?.saved || null"
        (delete)="onDelete($event)"
        (update)="onUpdate($event)"
      ></table>
    </div>
  `,
  standalone: true,
  imports: [
    CommonModule,
    TuiNotificationModule,
    TuiButtonModule,
    BackupsPhysicalComponent,
    BackupsTargetsComponent,
    TuiFadeModule,
  ],
})
export class BackupsTargetsModal implements OnInit {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly formDialog = inject(FormDialogService)
  private readonly loader = inject(LoadingService)

  readonly loading$ = new BehaviorSubject(true)

  targets?: RR.GetBackupTargetsRes

  ngOnInit() {
    this.refresh()
  }

  async refresh() {
    this.loading$.next(true)
    this.targets = undefined

    try {
      this.targets = await this.api.getBackupTargets({})
    } catch (e: any) {
      this.errorService.handleError(e)
      this.targets = { 'unknown-disks': [], saved: [] }
    } finally {
      this.loading$.next(false)
    }
  }

  async onDelete(id: string) {
    const loader = this.loader.open('Removing...').subscribe()

    try {
      await this.api.removeBackupTarget({ id })
      this.setTargets(this.targets?.saved.filter(a => a.id !== id))
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  async onUpdate(value: BackupTarget) {
    this.formDialog.open(FormPage, {
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
    this.formDialog.open(FormPage, {
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
                  this.targets?.saved.concat(response),
                  this.targets?.['unknown-disks'].filter(a => a !== disk),
                )
                return true
              }),
          },
        ],
      },
    })
  }

  async addRemote() {
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
    saved: BackupTarget[] = this.targets?.saved || [],
    unknown: UnknownDisk[] = this.targets?.['unknown-disks'] || [],
  ) {
    this.targets = { ['unknown-disks']: unknown, saved }
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
