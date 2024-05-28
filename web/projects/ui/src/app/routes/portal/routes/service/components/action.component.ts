import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiIconModule } from '@taiga-ui/experimental'

interface ActionItem {
  readonly icon: string
  readonly name: string
  readonly description: string
}

@Component({
  selector: '[action]',
  template: `
    <tui-icon [icon]="action.icon" />
    <div>
      <strong>{{ action.name }}</strong>
      <div>{{ action.description }}</div>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIconModule],
})
export class ServiceActionComponent {
  @Input({ required: true })
  action!: ActionItem
}
