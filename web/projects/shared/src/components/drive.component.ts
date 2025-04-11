import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { Component, Input } from '@angular/core'
import { UnitConversionPipesModule } from '../pipes/unit-conversion/unit-conversion.module'

@Component({
  standalone: true,
  selector: 'button[drive]',
  template: `
    <tui-icon icon="@tui.save" />
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
  imports: [TuiIcon, TuiTitle, UnitConversionPipesModule],
})
export class DriveComponent {
  @Input() drive!: {
    logicalname: string | null
    vendor: string | null
    model: string | null
    capacity: number
  }
}
