import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogOptions,
  TuiDialogService,
  TuiIcon,
  TuiLoader,
} from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupsStatusComponent } from '../components/status.component'
import { GetDisplayInfoPipe } from '../pipes/get-display-info.pipe'
import { BackupType } from '../types/backup-type'
import { TARGETS } from './targets.component'
import { getServerInfo } from 'src/app/utils/get-server-info'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    @if (loading()) {
      <tui-loader size="l" [textContent]="text" />
    } @else {
      <h3 class="g-title">Saved Targets</h3>
      @for (target of targets; track $index) {
        <button
          class="g-action"
          [disabled]="isDisabled(target)"
          (click)="context.completeWith(target)"
        >
          @if (target | getDisplayInfo; as displayInfo) {
            <tui-icon [icon]="displayInfo.icon" />
            <div>
              <strong>{{ displayInfo.name }}</strong>
              <backups-status
                [type]="context.data.type"
                [target]="target"
                [serverId]="serverId"
                ]
              />
              <div [style.color]="'var(--tui-text-secondary'">
                {{ displayInfo.description }}
                <br />
                {{ displayInfo.path }}
              </div>
            </div>
          }
        </button>
      } @empty {
        <p>No saved targets</p>
        <button tuiButton (click)="goToTargets()">Go to Targets</button>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiLoader,
    TuiButton,
    TuiIcon,
    BackupsStatusComponent,
    GetDisplayInfoPipe,
  ],
})
export class BackupsTargetModal {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly patch = inject(PatchDB<DataModel>)

  readonly context =
    inject<TuiDialogContext<BackupTarget, { type: BackupType }>>(
      POLYMORPHEUS_CONTEXT,
    )

  readonly loading = signal(true)
  readonly text =
    this.context.data.type === 'create'
      ? 'Loading Backup Targets'
      : 'Loading Backup Sources'

  serverId = ''
  targets: BackupTarget[] = []

  async ngOnInit() {
    try {
      this.serverId = (await getServerInfo(this.patch)).id
      this.targets = (await this.api.getBackupTargets({})).saved
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading.set(false)
    }
  }

  isDisabled(target: BackupTarget): boolean {
    return (
      !target.mountable ||
      (this.context.data.type === 'restore' && !target.startOs)
    )
  }

  goToTargets() {
    this.context.$implicit.complete()
    this.dialogs
      .open(TARGETS, { label: 'Backup Targets', size: 'l' })
      .subscribe()
  }
}

export const TARGET = new PolymorpheusComponent(BackupsTargetModal)

export const TARGET_CREATE: Partial<TuiDialogOptions<{ type: BackupType }>> = {
  label: 'Select Backup Target',
  data: { type: 'create' },
}

export const TARGET_RESTORE: Partial<TuiDialogOptions<{ type: BackupType }>> = {
  label: 'Select Backup Source',
  data: { type: 'restore' },
}
