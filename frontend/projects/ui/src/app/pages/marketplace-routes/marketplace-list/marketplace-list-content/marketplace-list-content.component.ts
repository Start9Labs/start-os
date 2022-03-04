import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { LocalPkg, MarketplacePkg } from '@start9labs/marketplace'

@Component({
  selector: 'marketplace-list-content',
  templateUrl: 'marketplace-list-content.component.html',
  styleUrls: ['./marketplace-list-content.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceListContentComponent {
  @Input()
  pkgs: MarketplacePkg[] | null = null

  @Input()
  localPkgs: Record<string, LocalPkg> = {}

  @Input()
  categories: Set<string> | null = null

  @Input()
  name = ''

  category = 'featured'
  query = ''

  isSelected(category: string) {
    return category === this.category && !this.query
  }

  switchCategory(category: string): void {
    this.category = category
    this.query = ''
  }
}
