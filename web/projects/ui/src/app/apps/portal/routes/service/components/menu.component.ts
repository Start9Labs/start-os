import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ToMenuPipe } from '../pipes/to-menu.pipe'
import { ServiceMenuItemComponent } from './menu-item.component'
import { RouterLink } from '@angular/router'

@Component({
  selector: 'service-menu',
  template: `
    <h3 class="g-title">Menu</h3>
    @for (menu of service | toMenu; track $index) {
      @if (menu.routerLink) {
        <a
          class="g-action"
          [serviceMenuItem]="menu"
          [routerLink]="menu.routerLink"
          [queryParams]="menu.params || {}"
        ></a>
      } @else {
        <button
          class="g-action"
          [serviceMenuItem]="menu"
          (click)="menu.action?.()"
        >
          @if (menu.name === 'Outbound Proxy') {
            <div [style.color]="color">{{ proxy }}</div>
          }
        </button>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ToMenuPipe, ServiceMenuItemComponent, RouterLink],
})
export class ServiceMenuComponent {
  @Input({ required: true })
  service!: PackageDataEntry

  get color(): string {
    return this.service.installed?.outboundProxy
      ? 'var(--tui-success-fill)'
      : 'var(--tui-warning-fill)'
  }

  get proxy(): string {
    switch (this.service.installed?.outboundProxy) {
      case 'primary':
        return 'System Primary'
      case 'mirror':
        return 'Mirror P2P'
      default:
        return this.service.installed?.outboundProxy?.proxyId || 'None'
    }
  }
}
