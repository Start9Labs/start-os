import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'

interface ActionItem {
  readonly icon: string
  readonly name: string
  readonly description: string
}

@Component({
  selector: '[action]',
  template: `
    <tui-svg [src]="action.icon"></tui-svg>
    <div>
      <strong>{{ action.name }}</strong>
      <div>{{ action.description }}</div>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiSvgModule],
})
export class ServiceActionComponent {
  @Input({ required: true })
  action!: ActionItem
}
