import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { EmverPipesModule } from '@start9labs/shared'
import { CommonModule } from '@angular/common'
import { TuiSvgModule } from '@taiga-ui/core'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  selector: '[serviceDependency]',
  template: `
    <img [src]="dep.icon" alt="" />
    <span [style.flex]="1">
      <strong>
        <tui-svg
          *ngIf="dep.errorText"
          src="tuiIconAlertTriangle"
          [style.color]="color"
        ></tui-svg>
        {{ dep.title }}
      </strong>
      <div>{{ dep.version | displayEmver }}</div>
      <div [style.color]="color">
        {{ dep.errorText || 'Satisfied' }}
      </div>
    </span>
    <div *ngIf="dep.actionText">
      {{ dep.actionText }}
      <tui-svg src="tuiIconArrowRight"></tui-svg>
    </div>
  `,
  styles: [
    `
      img {
        width: 1.5rem;
        height: 1.5rem;
        border-radius: 100%;
      }

      tui-svg {
        width: 1rem;
        height: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [EmverPipesModule, CommonModule, TuiSvgModule],
})
export class ServiceDependencyComponent {
  @Input({ required: true, alias: 'serviceDependency' })
  dep!: DependencyInfo

  get color(): string {
    return this.dep.errorText
      ? 'var(--tui-warning-fill)'
      : 'var(--tui-success-fill)'
  }
}
