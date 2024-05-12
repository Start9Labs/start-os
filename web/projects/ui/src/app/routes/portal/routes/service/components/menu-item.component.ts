import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIconModule } from '@taiga-ui/experimental'
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
    <tui-icon icon="tuiIconChevronRight" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIconModule],
})
export class ServiceMenuItemComponent {
  @Input({ required: true, alias: 'serviceMenuItem' })
  menu!: ServiceMenu
}
