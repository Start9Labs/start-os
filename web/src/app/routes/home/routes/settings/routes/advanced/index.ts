import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { Help } from 'src/app/directives/help'

import { AdvancedAside } from './aside'

@Component({
  template: `
    <advanced-aside *help />
    <section tuiCardLarge>
      <footer>
        <button tuiButton>Launch LuCI Interface</button>
        <button tuiButton>Download Support Diagnostics</button>
        <button tuiButton>Factory Reset</button>
      </footer>
    </section>
  `,
  styles: `
    footer {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;
    }
  `,
  host: { class: 'g-page' },
  imports: [AdvancedAside, Help, TuiCardLarge, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Advanced {}
