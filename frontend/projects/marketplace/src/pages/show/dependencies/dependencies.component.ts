import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { MarketplacePkg } from '../../../types'

@Component({
  selector: 'marketplace-dependencies',
  templateUrl: 'dependencies.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DependenciesComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  getImg(key: string): string {
    // @TODO fix when registry api is updated to include mimetype in icon url
    return 'data:image/png;base64,' + this.pkg['dependency-metadata'][key].icon
  }
}
