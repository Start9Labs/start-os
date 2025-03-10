import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'router-info',
  template: `
    <tui-notification [appearance]="enabled ? 'positive' : 'warning'">
      @if (enabled) {
        <strong>UPnP Enabled!</strong>
        <p>
          The ports below have been
          <i>automatically</i>
          forwarded in your router.
        </p>
        If you are running multiple servers, you may want to override specific
        ports to suite your needs.
        <a
          tuiLink
          href="https://docs.start9.com/latest/user-manual/port-forwards/upnp#override"
          target="_blank"
          rel="noreferrer"
        >
          View instructions
        </a>
      } @else {
        <strong>UPnP Disabled</strong>
        <p>
          Below are a list of ports that must be
          <i>manually</i>
          forwarded in your router in order to enable clearnet access.
        </p>
        Alternatively, you can enable UPnP in your router for automatic
        configuration.
        <a
          tuiLink
          href="https://docs.start9.com/latest/user-manual/port-forwards/manual"
          target="_blank"
          rel="noreferrer"
        >
          View instructions
        </a>
      }
    </tui-notification>
  `,
  styles: ['strong { font-size: 1rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiNotification, TuiLink],
})
export class RouterInfoComponent {
  @Input()
  enabled = false
}
