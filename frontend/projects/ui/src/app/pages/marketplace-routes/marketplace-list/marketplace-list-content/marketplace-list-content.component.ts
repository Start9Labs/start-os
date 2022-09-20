import {
  ChangeDetectionStrategy,
  Component,
  Input,
  OnInit,
} from '@angular/core'
import { MarketplacePkg, Marketplace } from '@start9labs/marketplace'
import { MarkdownPipe } from '@start9labs/shared'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getUrlDomain } from '../../../../util/web.util'
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
  details: Marketplace | null = null

  category = 'featured'
  query = ''
  name = ''
  description = ''
  color = ''

  ngOnInit(): void {
    if (this.details) {
      this.name = this.details.name

      switch (getUrlDomain(this.details.url)) {
        case 'registry.start9.com':
          this.color = 'success'
          this.description = this.markdown.transform(
            'Services in this marketplace are packaged and maintained by the Start9 team. If you experience an issue or have a questions related to a service in this marketplace, one of our dedicated support staff will be happy to assist you.',
          )
          break
        case 'beta-registry-0-3.start9labs.com':
          this.color = 'primary'
          this.description = this.markdown.transform(
            'Services in this marketplace are undergoing active testing and may contain bugs. <b>Install at your own risk</b>. If you discover a bug or have a suggestion for improvement, please report it to the Start9 team in our community testing channel on Matrix.',
          )
          break
        case 'community.start9labs.com':
          this.color = 'tertiary'
          this.description = this.markdown.transform(
            'Services in this marketplace are packaged and maintained by members of the Start9 community. <b>Install at your own risk</b>. If you experience an issue or have a question related to a service in this marketplace, please reach out to the package developer for assistance.',
          )
          break
        default:
          // alt marketplace
          this.color = 'warning'
          this.description = this.markdown.transform(
            'Warning. This is an <b>Alternative</b> Marketplace. Start9 cannot verify the integrity or functionality of services in this marketplace, and they may cause harm to your system. <b>Install at your own risk</b>.',
          )
          break
      }
    }
  }

  onCategoryChange(category: string): void {
    this.category = category
    this.query = ''
  }
}
