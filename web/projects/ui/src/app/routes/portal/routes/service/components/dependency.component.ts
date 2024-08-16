import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ExverPipesModule } from '@start9labs/shared'
import { TuiIcon } from '@taiga-ui/core'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  selector: '[serviceDependency]',
  template: `
    <img [src]="dep.icon" alt="" />
    <span [style.flex]="1">
      <strong>
        @if (dep.errorText) {
          <tui-icon icon="@tui.triangle-alert" [style.color]="color" />
        }
        {{ dep.title }}
      </strong>
      <div>{{ dep.version }}</div>
      <div [style.color]="color">{{ dep.errorText || 'Satisfied' }}</div>
    </span>
    @if (dep.actionText) {
      <div>
        {{ dep.actionText }}
        <tui-icon icon="@tui.arrow-right" />
      </div>
    }
  `,
  styles: [
    `
      img {
        width: 1.5rem;
        height: 1.5rem;
        border-radius: 100%;
      }

      tui-icon {
        font-size: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ExverPipesModule, TuiIcon],
})
export class ServiceDependencyComponent {
  @Input({ required: true, alias: 'serviceDependency' })
  dep!: DependencyInfo

  get color(): string {
    return this.dep.errorText
      ? 'var(--tui-status-warning)'
      : 'var(--tui-status-positive)'
  }
}
