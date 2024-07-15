import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'

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
  imports: [TuiIcon],
})
export class ServiceActionComponent {
  @Input({ required: true })
  action!: ActionItem
}
