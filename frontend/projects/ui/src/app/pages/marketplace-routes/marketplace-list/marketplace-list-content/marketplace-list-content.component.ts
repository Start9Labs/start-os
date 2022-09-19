import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnInit,
} from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { MarkdownPipe, MarkdownPipeModule } from '@start9labs/shared'

import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'marketplace-list-content',
  templateUrl: 'marketplace-list-content.component.html',
  styleUrls: ['./marketplace-list-content.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  providers: [MarkdownPipe],
})
export class MarketplaceListContentComponent implements OnInit {
  constructor(private markdown: MarkdownPipe) {}

  @Input()
  pkgs: MarketplacePkg[] | null = null

  @Input()
  localPkgs: Record<string, PackageDataEntry> = {}

  @Input()
  categories: Set<string> | null = null

  @Input()
  name = ''

  @Input()
  description: string | undefined = undefined

  category = 'featured'
  query = ''

  ngOnInit(): void {
    this.description = this.description
      ? this.markdown.transform(this.description)
      : undefined
  }
  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
  }
}
