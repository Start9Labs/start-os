import {
  ChangeDetectionStrategy,
  Component,
  inject,
  output,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { UnitConversionPipesModule } from '@start9labs/shared'
import {
  TuiAlertService,
  TuiButton,
  TuiIcon,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiTooltip } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { DiskBackupTarget } from 'src/app/services/api/api.types'
import { BackupService, MappedBackupTarget } from './backup.service'
import { BackupStatusComponent } from './status.component'

@Component({
  standalone: true,
  selector: '[physicalFolders]',
  template: `
    <header>
      Physical Drives
      <tui-icon [tuiTooltip]="drives" />
      <ng-template #drives><ng-content /></ng-template>
    </header>

    <tui-notification appearance="warning">
      Warning. Do not use this option if you are using a Raspberry Pi with an
      external SSD. The Raspberry Pi does not support more than one external
      drive without additional power and can cause data corruption.
    </tui-notification>

    @for (target of service.drives(); track $index) {
      <button tuiCell (click)="select(target)">
        <tui-icon icon="@tui.save" />
        <span tuiTitle>
          <strong>{{ target.entry.label || target.entry.logicalname }}</strong>
          <span tuiSubtitle [backupStatus]="target.hasAnyBackup"></span>
          <span tuiSubtitle>
            {{ target.entry.vendor || 'Unknown Vendor' }} -
            {{ target.entry.model || 'Unknown Model' }}
          </span>
          <span tuiSubtitle>
            <b>Capacity:</b>
            {{ target.entry.capacity | convertBytes }}
          </span>
        </span>
      </button>
    } @empty {
      <app-placeholder icon="@tui.save-off">
        No drives detected
        <button
          tuiButton
          iconStart="@tui.refresh-cw"
          (click)="service.getBackupTargets()"
        >
          Refresh
        </button>
      </app-placeholder>
    }
  `,
  styles: `
    tui-notification {
      margin: 0.5rem 0 0.75rem;
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiCell,
    TuiIcon,
    TuiTitle,
    TuiTooltip,
    TuiNotification,
    UnitConversionPipesModule,
    PlaceholderComponent,
    BackupStatusComponent,
  ],
})
export class BackupPhysicalComponent {
  private readonly alerts = inject(TuiAlertService)
  private readonly type = inject(ActivatedRoute).snapshot.data['type']

  readonly service = inject(BackupService)
  readonly physicalFolders = output<MappedBackupTarget<DiskBackupTarget>>()

  select(target: MappedBackupTarget<DiskBackupTarget>) {
    if (this.type === 'restore' && !target.hasAnyBackup) {
      this.alerts
        .open('Drive partition does not contain a valid backup', {
          appearance: 'negative',
        })
        .subscribe()
    } else {
      this.physicalFolders.emit(target)
    }
  }
}
