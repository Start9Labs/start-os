import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiTitle } from '@taiga-ui/core'

interface ActionItem {
  readonly name: string
  readonly description: string
  readonly icon?: string
  readonly visibility?: T.ActionVisibility
}

@Component({
  selector: '[action]',
  template: `
    <div tuiTitle>
      <strong>{{ action.name }}</strong>
      <div tuiSubtitle [innerHTML]="action.description"></div>
      @if (disabled) {
        <div tuiSubtitle class="g-warning">{{ disabled }}</div>
      }
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiTitle],
  host: {
    '[disabled]': '!!disabled',
  },
})
export class ServiceActionComponent {
  @Input({ required: true })
  action!: ActionItem

  get disabled() {
    return (
      typeof this.action.visibility === 'object' &&
      this.action.visibility.disabled
    )
  }
}
