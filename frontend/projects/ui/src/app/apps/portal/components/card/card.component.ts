import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostListener,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { TickerModule } from '@start9labs/shared'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { NavigationService } from '../navigation/navigation.service'
import { Action, ActionsComponent } from '../actions/actions.component'
import { toRouterLink } from '../../utils/to-router-link'

@Component({
  selector: '[appCard]',
  templateUrl: 'card.component.html',
  styleUrls: ['card.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    RouterLink,
    TuiButtonModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    TickerModule,
    ActionsComponent,
  ],
})
export class CardComponent {
  private readonly navigation = inject(NavigationService)

  @Input()
  id = ''

  @Input()
  icon = ''

  @Input()
  title = ''

  @Input()
  actions: Record<string, readonly Action[]> = {}

  @HostListener('click')
  onClick() {
    const { id, icon, title } = this
    const routerLink = toRouterLink(id)

    this.navigation.addTab({ icon, title, routerLink })
  }
}
