import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { MarketplacePkg } from '@start9labs/marketplace'
import { DOCUMENT } from '@angular/common'
import { DependentInfo } from 'src/app/types/dependent-info'

@Component({
  selector: 'marketplace-show-dependent',
  templateUrl: 'marketplace-show-dependent.component.html',
  styleUrls: ['marketplace-show-dependent.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowDependentComponent {
  @Input()
  pkg: MarketplacePkg

  readonly dependentInfo?: DependentInfo =
    this.document.defaultView?.history.state?.dependentInfo

  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  get title(): string {
    return this.pkg?.manifest.title || ''
  }

  get version(): string {
    return this.pkg?.manifest.version || ''
  }
}
