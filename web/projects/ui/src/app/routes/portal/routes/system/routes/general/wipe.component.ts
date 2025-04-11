import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiLabel } from '@taiga-ui/core'
import { TuiCheckbox } from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import SystemGeneralComponent from './general.component'

@Component({
  standalone: true,
  template: `
    <p>
      @if (isTor) {
        You are currently connected over Tor. If you reset the Tor daemon, you
        will lose connectivity until it comes back online.
      } @else {
        Reset Tor?
      }
    </p>
    <p>
      Optionally wipe state to forcibly acquire new guard nodes. It is
      recommended to try without wiping state first.
    </p>
    <label tuiLabel>
      <input type="checkbox" tuiCheckbox [(ngModel)]="component.wipe" />
      Wipe state
    </label>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLabel, FormsModule, TuiCheckbox],
})
export class SystemWipeComponent {
  readonly isTor = inject(ConfigService).isTor()
  readonly component = inject(SystemGeneralComponent)
}
