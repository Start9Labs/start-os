import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiCell } from '@taiga-ui/layout'
import { ServiceMenu } from '../pipes/to-menu.pipe'

@Component({
  selector: '[serviceMenuItem]',
  template: `
    <tui-icon [icon]="menu.icon" />
    <span tuiTitle [style.flex]="1">
      <strong>{{ menu.name }}</strong>
      <span tuiSubtitle>
        {{ menu.description }}
      </span>
      <ng-content />
    </span>
    <tui-icon icon="@tui.chevron-right" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIcon, TuiTitle],
  hostDirectives: [TuiCell],
})
export class ServiceMenuItemComponent {
  @Input({ required: true, alias: 'serviceMenuItem' })
  menu!: ServiceMenu
}
