import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { TuiIcon, TuiLoader } from '@taiga-ui/core'

@Component({
  selector: 'port-check-icon',
  template: `
    @if (loading()) {
      <tui-loader size="s" />
    } @else {
      @let res = result();
      @if (res) {
        @if (!res.openInternally) {
          <tui-icon class="g-warning" icon="@tui.alert-triangle" />
        } @else if (!res.openExternally) {
          <tui-icon class="g-negative" icon="@tui.x" />
        } @else {
          <tui-icon class="g-positive" icon="@tui.check" />
        }
      } @else {
        <tui-icon class="g-secondary" icon="@tui.minus" />
      }
    }
  `,
  styles: `
    tui-icon {
      font-size: 1.3rem;
      vertical-align: text-bottom;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, TuiLoader],
})
export class PortCheckIconComponent {
  readonly result = input<T.CheckPortRes>()
  readonly loading = input(false)
}
