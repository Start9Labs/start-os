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
}
