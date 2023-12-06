import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiNotificationModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCellModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'

@Component({
  selector: 'settings-http',
  template: `
    <tui-notification status="warning">
      <div tuiCell [style.padding]="0">
        <div tuiTitle>
          Http detected
          <div tuiSubtitle>
            <div>
              Tor is faster over https.
              <a
                href="https://docs.start9.com/0.3.5.x/user-manual/trust-ca"
                target="_blank"
                rel="noreferrer"
              >
                Download and trust your server's Root CA
              </a>
              , then switch to https.
            </div>
          </div>
        </div>
        <ng-content />
      </div>
    </tui-notification>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    TuiButtonModule,
    TuiCellModule,
    TuiNotificationModule,
    TuiTitleModule,
  ],
})
export class SettingsHttpsComponent {}
