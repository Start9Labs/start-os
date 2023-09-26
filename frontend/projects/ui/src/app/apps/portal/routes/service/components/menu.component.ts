import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiSvgModule } from '@taiga-ui/core'
import { ServiceMenu } from '../pipes/to-menu.pipe'

@Component({
  selector: '[serviceMenu]',
  template: `
    <tui-svg [src]="menu.icon"></tui-svg>
    <div [style.flex]="1">
      <strong>{{ menu.name }}</strong>
      <div>
        {{ menu.description }}
        <ng-content></ng-content>
      </div>
    </div>
    <tui-svg src="tuiIconChevronRightLarge"></tui-svg>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiSvgModule],
})
export class ServiceMenuComponent {
  @Input({ required: true, alias: 'serviceMenu' })
  menu!: ServiceMenu
}
