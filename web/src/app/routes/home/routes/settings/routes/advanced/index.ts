import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'

import { AdvancedAside } from './aside'

@Component({
  template: `
    <advanced-aside *help />
    <section tuiCardLarge [style.align-items]="'start'">
      <button tuiButton>Launch LuCI Interface</button>
      <button tuiButton>Download Support Diagnostics</button>
      <button tuiButton>Factory Reset</button>
    </section>
  `,
  styles: `
    :host {
      max-width: 50rem;
    }
  `,
  host: { class: 'g-page' },
  imports: [AdvancedAside, Help, TuiCardLarge, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Advanced {}
