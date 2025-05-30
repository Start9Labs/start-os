import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiLink, TuiNotification } from '@taiga-ui/core'
import { DocsLinkDirective } from 'projects/shared/src/public-api'

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
        <a tuiLink docsLink href="/@TODO">View instructions</a>
      } @else {
        <strong>UPnP Disabled</strong>
        <p>
          Below are a list of ports that must be
          <i>manually</i>
          forwarded in your router in order to enable clearnet access.
        </p>
        Alternatively, you can enable UPnP in your router for automatic
        configuration.
        <a tuiLink docsLink href="/@TODO">View instructions</a>
      }
    </tui-notification>
  `,
  styles: `
    strong {
      font-size: 1rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiNotification, TuiLink, DocsLinkDirective],
})
export class RouterInfoComponent {
  @Input()
  enabled = false
}
