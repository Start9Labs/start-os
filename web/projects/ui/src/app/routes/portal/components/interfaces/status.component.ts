import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  standalone: true,
  selector: 'interface-status',
  template: `
    <tui-badge
      size="l"
      [iconStart]="public() ? '@tui.globe' : '@tui.lock'"
      [style.vertical-align.rem]="-0.125"
      [style.margin]="'0 0.25rem -0.25rem'"
      [appearance]="public() ? 'positive' : 'negative'"
    >
      {{ public() ? 'Public' : 'Private' }}
    </tui-badge>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadge],
})
export class InterfaceStatusComponent {
  readonly public = input(false)
}
