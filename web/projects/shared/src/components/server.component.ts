import { DatePipe } from '@angular/common'
import { Component, input } from '@angular/core'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { StartOSDiskInfo } from '../types/api'

@Component({
  selector: 'button[server]',
  template: `
    <tui-icon icon="@tui.save" />
    <span tuiTitle>
      <strong>{{ server().hostname.replace('.local', '') }}.local</strong>
      <span tuiSubtitle>
        <b>StartOS Version</b>
        : {{ server().version }}
      </span>
      <span tuiSubtitle>
        <b>Created</b>
        : {{ server().timestamp | date: 'medium' }}
      </span>
    </span>
  `,
  styles: `
    :host {
      border-radius: var(--tui-radius-l);
    }
  `,
  host: { class: 'g-stretch' },
  hostDirectives: [TuiCell],
  imports: [DatePipe, TuiIcon, TuiTitle],
})
export class ServerComponent {
  readonly server = input.required<StartOSDiskInfo>()
}
