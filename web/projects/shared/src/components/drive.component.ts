import { Component, Input } from '@angular/core'
import { TuiIconModule, TuiTitleModule } from '@taiga-ui/experimental'
import { UnitConversionPipesModule } from '../pipes/unit-conversion/unit-conversion.module'

@Component({
  standalone: true,
  selector: 'button[drive]',
  template: `
    <tui-icon icon="tuiIconSave" />
    <span tuiTitle>
      <strong>{{ drive.logicalname }}</strong>
      <span tuiSubtitle>
        {{ drive.vendor || 'Unknown Vendor' }} -
        {{ drive.model || 'Unknown Model' }}
      </span>
      <span tuiSubtitle>Capacity: {{ drive.capacity | convertBytes }}</span>
      <ng-content />
    </span>
  `,
  imports: [TuiIconModule, TuiTitleModule, UnitConversionPipesModule],
})
export class DriveComponent {
  @Input() drive!: {
    logicalname: string | null
    vendor: string | null
    model: string | null
    capacity: number
  }
}
