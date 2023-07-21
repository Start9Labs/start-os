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
import {
  NavigationItem,
  NavigationService,
} from '../navigation/navigation.service'

@Component({
  selector: '[appCard]',
  templateUrl: 'card.component.html',
  styleUrls: ['card.component.scss'],
  standalone: true,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    RouterLink,
    TuiButtonModule,
    TuiHostedDropdownModule,
    TuiDataListModule,
    TuiSvgModule,
    TickerModule,
  ],
})
export class CardComponent {
  private readonly navigation = inject(NavigationService)

  @Input({ required: true })
  appCard!: NavigationItem

  @HostListener('click')
  onClick() {
    this.navigation.addTab(this.appCard)
  }
}
