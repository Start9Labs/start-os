import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { RouterModule } from '@angular/router'
import { LocalizePipe, TickerComponent } from '@start9labs/shared'
import { TuiAvatar } from '@taiga-ui/kit'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-item',
  templateUrl: 'item.component.html',
  styleUrls: ['item.component.scss'],
  imports: [
    CommonModule,
    RouterModule,
    TickerComponent,
    LocalizePipe,
    TuiAvatar,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ItemComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  determineIcon(): string {
    return this.pkg.icon
      ? this.pkg.icon
      : 'assets/img/service-icons/fallback.png'
  }
}
