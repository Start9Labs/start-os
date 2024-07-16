import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { MarketplacePkg } from '../../../types'
import { AbstractPkgFlavorService } from '../../../services/pkg-implementation.service'

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

  toggleImplementation: boolean = false

  constructor(private readonly pkgFlavorService: AbstractPkgFlavorService) {}

  switchImplementation() {
    this.toggleImplementation = !this.toggleImplementation
    this.pkgFlavorService.toggleFlavorStatus(this.toggleImplementation)
    this.version.emit(this.pkg.flavorVersion!)
  }
}
