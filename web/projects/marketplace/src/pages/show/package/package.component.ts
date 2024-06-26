import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  Input,
  Output,
} from '@angular/core'
import { MarketplacePkg } from '../../../types'
import { AbstractPkgImplementationService } from '../../../services/pkg-implementation.service'

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

  constructor(
    private readonly pkgImplService: AbstractPkgImplementationService,
  ) {}

  switchImplementation() {
    this.toggleImplementation = !this.toggleImplementation
    this.pkgImplService.toggleAltStatus(this.toggleImplementation)
    this.version.emit(this.pkg.altVersion!)
  }
}
