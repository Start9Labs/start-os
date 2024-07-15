import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiNotification } from '@taiga-ui/core'

@Component({
  selector: 'router-info',
  template: `
    <tui-notification [status]="enabled ? 'success' : 'warning'">
      <ng-container *ngIf="enabled; else disabled">
        <strong>UPnP Enabled!</strong>
        <p>
          The ports below have been
          <i>automatically</i>
          forwarded in your router.
        </p>
        If you are running multiple servers, you may want to override specific
        ports to suite your needs.
        <a
          href="https://docs.start9.com/latest/user-manual/port-forwards/upnp#override"
          target="_blank"
          rel="noreferrer"
        >
          View instructions
        </a>
      </ng-container>
      <ng-template #disabled>
        <strong>UPnP Disabled</strong>
        <p>
          Below are a list of ports that must be
          <i>manually</i>
          forwarded in your router in order to enable clearnet access.
        </p>
        Alternatively, you can enable UPnP in your router for automatic
        configuration.
        <a
          href="https://docs.start9.com/latest/user-manual/port-forwards/manual"
          target="_blank"
          rel="noreferrer"
        >
          View instructions
        </a>
      </ng-template>
    </tui-notification>
  `,
  styles: ['strong { font-size: 1rem }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiNotification],
})
export class RouterInfoComponent {
  @Input()
  enabled = false
}
