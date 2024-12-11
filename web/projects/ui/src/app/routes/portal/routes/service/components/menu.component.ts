import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { ToMenuPipe } from '../pipes/to-menu.pipe'
import { ServiceMenuItemComponent } from './menu-item.component'

@Component({
  selector: 'service-menu',
  template: `
    @for (menu of pkg | toMenu; track $index) {
      @if (menu.routerLink) {
        <a
          [serviceMenuItem]="menu"
          [routerLink]="menu.routerLink"
          [queryParams]="menu.params || {}"
        ></a>
      } @else {
        <button [serviceMenuItem]="menu" (click)="menu.action?.()">
          @if (menu.name === 'Outbound Proxy') {
            <div tuiSubtitle [style.color]="color">
              {{ pkg.outboundProxy || 'None' }}
            </div>
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
  pkg!: PackageDataEntry

  get color(): string {
    return this.pkg.outboundProxy
      ? 'var(--tui-status-positive)'
      : 'var(--tui-status-warning)'
  }
}
