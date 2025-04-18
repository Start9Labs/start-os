import {
  ChangeDetectionStrategy,
  Component,
  inject,
  output,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import {
  DialogService,
  i18nPipe,
  UnitConversionPipesModule,
} from '@start9labs/shared'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiTooltip } from '@taiga-ui/kit'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { DiskBackupTarget } from 'src/app/services/api/api.types'
import { BackupService, MappedBackupTarget } from './backup.service'
import { BackupStatusComponent } from './status.component'

@Component({
  standalone: true,
  selector: '[physicalFolders]',
  template: `
    <header>
      {{ 'Physical Drives' | i18n }}
      <tui-icon [tuiTooltip]="drives" />
      <ng-template #drives><ng-content /></ng-template>
    </header>

    <table [appTable]="['Status', 'Name', 'Model', 'Capacity']">
      @for (target of service.drives(); track $index) {
        <tr
          tabindex="0"
          (click)="select(target)"
          (keydown.enter)="select(target)"
        >
          <td><span [backupStatus]="target.hasAnyBackup"></span></td>
          <td class="name">
            {{ target.entry.label || target.entry.logicalname }}
          </td>
          <td>
            {{ target.entry.vendor || 'Unknown Vendor' }} -
            {{ target.entry.model || 'Unknown Model' }}
          </td>
          <td>{{ target.entry.capacity | convertBytes }}</td>
        </tr>
      } @empty {
        <tr>
          <td colspan="4">
            <app-placeholder icon="@tui.save-off">
              {{ 'No drives detected' | i18n }}
              <button
                tuiButton
                iconStart="@tui.refresh-cw"
                (click)="service.getBackupTargets()"
              >
                {{ 'Refresh' | i18n }}
              </button>
            </app-placeholder>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    tr {
      cursor: pointer;
      @include transition(background);

      @media ($tui-mouse) {
        &:hover {
          background: var(--tui-background-neutral-1-hover);
        }
      }
    }

    td:first-child {
      width: 13rem;
    }

    :host-context(tui-root._mobile) {
      tr {
        max-width: 18rem;
        grid-template-columns: min-content 2rem;
        white-space: nowrap;
      }

      td {
        grid-column: span 2;

        &:first-child {
          font-size: 0;
          width: auto;
          grid-area: 1 / 2;
          place-content: center;
          margin: 0 0.5rem;
        }
      }

      .name {
        color: var(--tui-text-primary);
        font: var(--tui-font-text-m);
        font-weight: bold;
        grid-column: 1;
        max-width: 12rem;
      }
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiIcon,
    TuiTooltip,
    UnitConversionPipesModule,
    PlaceholderComponent,
    BackupStatusComponent,
    TableComponent,
    i18nPipe,
  ],
})
export class BackupPhysicalComponent {
  private readonly dialog = inject(DialogService)
  private readonly type = inject(ActivatedRoute).snapshot.data['type']

  readonly service = inject(BackupService)
  readonly physicalFolders = output<MappedBackupTarget<DiskBackupTarget>>()

  select(target: MappedBackupTarget<DiskBackupTarget>) {
    if (this.type === 'restore' && !target.hasAnyBackup) {
      this.dialog
        .openAlert('Drive partition does not contain a valid backup', {
          appearance: 'negative',
        })
        .subscribe()
    } else {
      this.physicalFolders.emit(target)
    }
  }
}
