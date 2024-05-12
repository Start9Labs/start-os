import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { DependencyInfo } from '../types/dependency-info'
import { ServiceDependencyComponent } from './dependency.component'

@Component({
  selector: 'service-dependencies',
  template: `
    @for (dep of dependencies; track $index) {
      <button
        class="g-action"
        [serviceDependency]="dep"
        (click)="dep.action()"
      ></button>
    }

    @if (!dependencies.length) {
      No dependencies
    }
  `,
  styles: ':host { display: block; min-height: var(--tui-height-s) }',
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ServiceDependencyComponent],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  dependencies: readonly DependencyInfo[] = []
}
