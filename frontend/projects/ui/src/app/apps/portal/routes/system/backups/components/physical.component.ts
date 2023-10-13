import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiButtonModule, TuiSvgModule } from '@taiga-ui/core'
import { UnknownDisk } from 'src/app/services/api/api.types'
import { IonicModule } from '@ionic/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'

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
      <tr *ngFor="let disk of backupsPhysical; else: loading; empty: blank">
        <td>
          {{ disk.vendor || 'unknown make' }},
          {{ disk.model || 'unknown model' }}
        </td>
        <td>{{ disk.label }}</td>
        <td>{{ disk.capacity | convertBytes }}</td>
        <td>{{ disk.used ? (disk.used | convertBytes) : 'Unknown' }}</td>
        <td>
          <button
            tuiButton
            size="xs"
            icon="tuiIconPlus"
            (click)="add.emit(disk)"
          >
            Save
          </button>
        </td>
      </tr>
      <ng-template #loading>
        <tr>
          <td colspan="5">
            <div class="tui-skeleton">Loading</div>
          </td>
        </tr>
      </ng-template>
      <ng-template #blank>
        <tr>
          <td colspan="5">
            To add a new physical backup target, connect the drive and click
            refresh.
          </td>
        </tr>
      </ng-template>
    </tbody>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiForModule,
    TuiSvgModule,
    TuiButtonModule,
    IonicModule,
    UnitConversionPipesModule,
  ],
})
export class BackupsPhysicalComponent {
  @Input()
  backupsPhysical: readonly UnknownDisk[] | null = null

  @Output()
  readonly add = new EventEmitter<UnknownDisk>()
}
