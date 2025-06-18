import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import { TuiBadge } from '@taiga-ui/kit'

@Component({
  selector: 'interface-status',
  template: `
    <tui-badge
      size="l"
      [iconStart]="public() ? '@tui.globe' : '@tui.lock'"
      [appearance]="public() ? 'positive' : 'negative'"
    >
      {{ public() ? ('Public' | i18n) : ('Private' | i18n) }}
    </tui-badge>
  `,
  styles: `
    :host {
      display: inline-flex;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiBadge, i18nPipe],
})
export class InterfaceStatusComponent {
  readonly public = input(false)
}
