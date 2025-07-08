import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { TuiButton } from '@taiga-ui/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { UnknownDisk } from 'src/app/services/api/api.types'

@Component({
  selector: 'table[backupsPhysical]',
  template: `
    <thead>
      <tr>
        <th>Make/Model</th>
        <th>Label</th>
        <th>Capacity</th>
        <th>Used</th>
        <th [style.width.rem]="4.25"></th>
      </tr>
    </thead>
    <tbody>
      @for (disk of backupsPhysical; track $index) {
        <tr>
          <td class="model">
            {{ disk.vendor || 'unknown make' }},
            {{ disk.model || 'unknown model' }}
          </td>
          <td class="title">{{ disk.label }}</td>
          <td class="capacity">{{ disk.capacity | convertBytes }}</td>
          <td class="used">
            {{ disk.used ? (disk.used | convertBytes) : 'Unknown' }}
          </td>
          <td class="actions">
            <button
              tuiButton
              size="xs"
              iconStart="@tui.plus"
              (click)="add.emit(disk)"
            >
              Save
            </button>
          </td>
        </tr>
      } @empty {
        @if (backupsPhysical) {
          <tr>
            <td colspan="5">
              To add a new physical backup target, connect the drive and click
              refresh.
            </td>
          </tr>
        } @else {
          <tr>
            <td colspan="5"><div [tuiSkeleton]="true">Loading</div></td>
          </tr>
        }
      }
    </tbody>
  `,
  styles: `
    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: 1fr 1fr;
      }

      td:only-child {
        grid-column: span 2;
      }

      .model {
        order: 1;
        white-space: nowrap;
        color: var(--tui-text-secondary);
      }

      .actions {
        order: 2;
        padding: 0;
        text-align: right;
      }

      .title {
        order: 3;
        grid-column: span 2;
        font-weight: bold;
        text-transform: uppercase;
      }

      .capacity {
        order: 4;

        &::before {
          content: 'Capacity: ';
        }
      }

      .used {
        order: 5;
        text-align: right;

        &::before {
          content: 'Used: ';
        }
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, UnitConversionPipesModule, TuiSkeleton],
})
export class BackupsPhysicalComponent {
  @Input()
  backupsPhysical: readonly UnknownDisk[] | null = null

  @Output()
  readonly add = new EventEmitter<UnknownDisk>()
}
