import { CommonModule, Location } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { RouterModule } from '@angular/router'
import { TuiButtonModule, TuiSvgModule } from '@taiga-ui/core'
import { NavigationItem, NavigationService } from './navigation.service'

@Component({
  selector: 'nav[appNavigation]',
  templateUrl: 'navigation.component.html',
  styleUrls: ['navigation.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, RouterModule, TuiButtonModule, TuiSvgModule],
})
export class NavigationComponent {
  private readonly location = inject(Location)
  private readonly navigation = inject(NavigationService)

  readonly tabs$ = this.navigation.getTabs()

  removeTab(tab: NavigationItem, active: boolean) {
    this.navigation.removeTab(tab)

    if (active) this.location.back()
  }
}
