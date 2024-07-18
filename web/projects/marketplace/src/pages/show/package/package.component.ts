import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { MarketplacePkg } from '../../../types'
import { AbstractPkgFlavorService } from '../../../services/pkg-flavor.service'

@Component({
  selector: 'marketplace-package',
  templateUrl: 'package.component.html',
  styleUrls: ['package.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PackageComponent {
  @Input()
  pkg!: MarketplacePkg

  @Output()
  version = new EventEmitter<string>()

  toggleFlavor: boolean = false

  constructor(private readonly pkgFlavorService: AbstractPkgFlavorService) {}

  async switchFlavor() {
    this.toggleFlavor = !this.toggleFlavor
    this.pkgFlavorService.toggleFlavorStatus(this.toggleFlavor)
    this.version.emit(
      this.toggleFlavor ? this.pkg.flavorVersion! : this.pkg.defaultVersion!,
    )
  }
}
