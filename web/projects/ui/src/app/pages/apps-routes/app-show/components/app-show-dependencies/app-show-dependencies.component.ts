import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { DependencyInfo } from '../../app-show.page'
import { T } from '@start9labs/start-sdk'
import {
  PackageDataEntry,
  StateInfo,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-dependencies',
  templateUrl: './app-show-dependencies.component.html',
  styleUrls: ['./app-show-dependencies.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowDependenciesComponent {
  @Input()
  dependencies: DependencyInfo[] = []

  @Input()
  allPkgs!: NonNullable<
    T.AllPackageData & Record<string, PackageDataEntry<StateInfo>>
  >

  @Input()
  pkg!: T.PackageDataEntry & { stateInfo: StateInfo }

  @Input()
  manifest!: T.Manifest
}
