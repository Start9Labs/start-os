import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnInit,
} from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

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
  localPkgs: Record<string, PackageDataEntry> = {}

  @Input()
  categories: Set<string> | null = null

  @Input()
  name = ''

  @Input()
  selected: string | null = null

  category = 'featured'
  query = ''

  onCategoryChange(category: string): void {
    console.log(this.selected)
    this.category = category
    this.query = ''
  }
}
