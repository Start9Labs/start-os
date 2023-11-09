import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router, RouterModule } from '@angular/router'
import { TuiSvgModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { NavigationService } from '../../services/navigation.service'
import { NavigationItem } from '../../types/navigation-item'

@Component({
  selector: 'nav[appNavigation]',
  templateUrl: 'navigation.component.html',
  styleUrls: ['navigation.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, RouterModule, TuiButtonModule, TuiSvgModule],
})
export class NavigationComponent {
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)

  readonly tabs$ = this.navigation.getTabs()

  removeTab(routerLink: string, active: boolean) {
    this.navigation.removeTab(routerLink)

    if (active) this.router.navigate(['/portal/desktop'])
  }
}
