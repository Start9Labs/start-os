import { TuiRoot } from '@taiga-ui/core'
import { Component, inject } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { PatchService } from './services/patch.service'
import { UpdateService } from './services/update.service'

@Component({
  selector: 'app-root',
  imports: [RouterOutlet, TuiRoot],
  template: '<tui-root><router-outlet /></tui-root>',
  styles: `
    :host {
      height: 100%;
      display: block;
    }

    tui-root {
      height: 100%;
    }
  `,
})
export class App {
  readonly subscription = inject(PatchService)
    .pipe(takeUntilDestroyed())
    .subscribe()

  readonly updates = inject(UpdateService)
}
