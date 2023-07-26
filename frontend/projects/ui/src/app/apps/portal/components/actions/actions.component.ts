import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiDataListModule, TuiSvgModule } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'

export interface Action {
  icon: string
  label: string
  action: () => void
}

@Component({
  selector: 'app-actions',
  templateUrl: './actions.component.html',
  styleUrls: ['./actions.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiDataListModule, TuiSvgModule, CommonModule],
})
export class ActionsComponent {
  @Input()
  actions: Record<string, readonly Action[]> = {}

  asIsOrder(a: any, b: any) {
    return 0
  }
}
