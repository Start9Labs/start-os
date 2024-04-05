import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ServiceDependencyComponent } from './dependency.component'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  selector: 'service-dependencies',
  template: `
    <h3 class="g-title">Dependencies</h3>
    <button
      *ngFor="let dep of dependencies"
      class="g-action"
      [serviceDependency]="dep"
      (click)="dep.action()"
    ></button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, ServiceDependencyComponent],
})
export class ServiceDependenciesComponent {
  @Input({ required: true })
  dependencies: readonly DependencyInfo[] = []
}
