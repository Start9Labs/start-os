import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiLabel } from '@taiga-ui/core'
import { TuiCheckbox } from '@taiga-ui/kit'
import { ConfigService } from 'src/app/services/config.service'
import SystemGeneralComponent from './general.component'
import { i18nPipe } from '@start9labs/shared'

@Component({
  template: `
    <p>
      @if (isTor) {
        {{
          'You are currently connected over Tor. If you reset the Tor daemon, you will lose connectivity until it comes back online.'
            | i18n
        }}
      } @else {
        {{ 'Reset Tor?' | i18n }}
      }
    </p>
    <p>
      {{
        'Optionally wipe state to forcibly acquire new guard nodes. It is recommended to try without wiping state first.'
          | i18n
      }}
    </p>
    <label tuiLabel>
      <input type="checkbox" tuiCheckbox [(ngModel)]="component.wipe" />
      {{ 'Wipe state' | i18n }}
    </label>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiLabel, FormsModule, TuiCheckbox, i18nPipe],
})
export class SystemWipeComponent {
  readonly isTor = inject(ConfigService).isTor()
  readonly component = inject(SystemGeneralComponent)
}
