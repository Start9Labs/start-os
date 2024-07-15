import { TuiIcon } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ServiceMenu } from '../pipes/to-menu.pipe'

@Component({
  selector: '[serviceMenuItem]',
  template: `
    <tui-icon [icon]="menu.icon" />
    <div [style.flex]="1">
      <strong>{{ menu.name }}</strong>
      <div>
        {{ menu.description }}
        <ng-content />
      </div>
    </div>
    <tui-icon icon="@tui.chevron-right" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIcon],
})
export class ServiceMenuItemComponent {
  @Input({ required: true, alias: 'serviceMenuItem' })
  menu!: ServiceMenu
}
