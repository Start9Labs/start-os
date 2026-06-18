import { ChangeDetectionStrategy, Component } from '@angular/core'
import { RouterOutlet } from '@angular/router'
import { TuiRoot } from '@taiga-ui/core'

@Component({
  selector: 'app-root',
  template: `
    <tui-root tuiTheme="dark">
      <router-outlet />
    </tui-root>
  `,
  styles: `
    tui-root {
      height: 100%;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiRoot, RouterOutlet],
})
export class AppComponent {}
