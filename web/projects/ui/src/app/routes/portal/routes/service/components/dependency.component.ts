import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { DependencyInfo } from '../types/dependency-info'

@Component({
  selector: '[serviceDependency]',
  template: `
    <img [src]="dep.icon" alt="" />
    <span tuiTitle>
      <strong>
        @if (dep.errorText) {
          <tui-icon icon="@tui.triangle-alert" [style.color]="color" />
        }
        {{ dep.title }}
      </strong>
      <span tuiSubtitle>{{ dep.version }}</span>
      <span tuiSubtitle="" [style.color]="color">
        {{ dep.errorText || 'Satisfied' }}
      </span>
    </span>
    @if (dep.actionText) {
      <span>
        {{ dep.actionText }}
        <tui-icon icon="@tui.arrow-right" />
      </span>
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
  host: {
    '(click)': 'dep.action()',
  },
  imports: [TuiIcon, TuiTitle],
  hostDirectives: [TuiCell],
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
