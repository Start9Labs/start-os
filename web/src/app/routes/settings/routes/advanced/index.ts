import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'

@Component({
  template: `
    <section class="g-form" tuiCardLarge [style.align-items]="'start'">
      <button tuiButton>Launch LuCI Interface</button>
      <button tuiButton>Download Support Diagnostics</button>
      <button tuiButton>Factory Reset</button>
    </section>
  `,
  host: { class: 'g-page' },
  imports: [TuiCardLarge, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Advanced {}
