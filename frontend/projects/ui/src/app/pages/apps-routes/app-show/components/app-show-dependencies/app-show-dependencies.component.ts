import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { DependencyInfo } from '../../app-show.page'

@Component({
  selector: 'app-show-dependencies',
  templateUrl: './app-show-dependencies.component.html',
  styleUrls: ['./app-show-dependencies.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowDependenciesComponent {
  @Input()
  dependencies: DependencyInfo[] = []
}
