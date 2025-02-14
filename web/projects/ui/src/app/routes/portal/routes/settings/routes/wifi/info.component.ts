import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'wifi-info',
  template: `
    <tui-notification>
      Adding WiFi credentials to StartOS allows you to remove the Ethernet cable
      and move the device anywhere you want. StartOS will automatically connect
      to available networks.
      <a
        href="https://docs.start9.com/latest/user-manual/wifi"
        target="_blank"
        rel="noreferrer"
      >
        View instructions
      </a>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification],
})
export class WifiInfoComponent {}
