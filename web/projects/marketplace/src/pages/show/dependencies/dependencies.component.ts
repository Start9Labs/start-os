import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { Dependency, MarketplacePkg } from '../../../types'
import { KeyValue } from '@angular/common'

@Component({
  selector: 'marketplace-dependencies',
  templateUrl: 'dependencies.component.html',
  styleUrls: ['./dependencies.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DependenciesComponent {
  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input({ required: true })
  dep!: KeyValue<string, Dependency>

  getImage(key: string): string {
    const icon = this.pkg['dependency-metadata'][key]?.icon
    // @TODO fix when registry api is updated to include mimetype in icon url
    return icon ? `data:image/png;base64,${icon}` : key.substring(0, 2)
  }

  getTitle(key: string): string {
    return this.pkg['dependency-metadata'][key]?.title || key
  }
}
