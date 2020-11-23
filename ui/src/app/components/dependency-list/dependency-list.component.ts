import { Component, Input } from '@angular/core'
import { BehaviorSubject } from 'rxjs'
import { AppDependency, BaseApp, isOptional } from 'src/app/models/app-types'

@Component({
  selector: 'dependency-list',
  templateUrl: './dependency-list.component.html',
  styleUrls: ['./dependency-list.component.scss'],
})
export class DependencyListComponent {
  @Input() depType: 'installed' | 'available' = 'available'
  @Input() hostApp: BaseApp
  @Input() dependencies: AppDependency[]
  dependenciesToDisplay: AppDependency[]
  @Input() $loading$: BehaviorSubject<boolean> = new BehaviorSubject(true)

  constructor () { }

  ngOnChanges () {
    this.dependenciesToDisplay = this.dependencies.filter(dep =>
      this.depType === 'available' ? !isOptional(dep) : true,
    )
  }

  ngOnInit () {
    this.dependenciesToDisplay = this.dependencies.filter(dep =>
      this.depType === 'available' ? !isOptional(dep) : true,
    )
  }
}
