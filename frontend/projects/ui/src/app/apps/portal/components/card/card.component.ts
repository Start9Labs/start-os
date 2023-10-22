import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  HostListener,
  inject,
  Input,
} from '@angular/core'
import {
  TuiBadgedContentModule,
  TuiBadgeNotificationModule,
} from '@taiga-ui/experimental'
import { RouterLink } from '@angular/router'
import { TickerModule } from '@start9labs/shared'
import {
  TuiButtonModule,
  TuiDataListModule,
  TuiHostedDropdownModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import { NavigationService } from '../../services/navigation.service'
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
    TuiBadgedContentModule,
    TuiBadgeNotificationModule,
    ActionsComponent,
  ],
})
export class CardComponent {
  private readonly navigation = inject(NavigationService)

  @Input({ required: true })
  id!: string

  @Input({ required: true })
  icon!: string

  @Input({ required: true })
  title!: string

  @Input()
  actions: Record<string, readonly Action[]> = {}

  @Input()
  badge: number | null = null

  get isService(): boolean {
    return !this.id.includes('/')
  }

  @HostListener('click')
  onClick() {
    const { id, icon, title } = this
    const routerLink = toRouterLink(id)

    this.navigation.addTab({ icon, title, routerLink })
  }

  @HostListener('pointerdown.prevent')
  onDown() {
    // Prevents Firefox from starting a native drag
  }
}
