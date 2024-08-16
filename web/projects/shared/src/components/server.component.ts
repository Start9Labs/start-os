import { DatePipe } from '@angular/common'
import { Component, inject, input } from '@angular/core'
import { TuiDialogService, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { StartOSDiskInfo } from '../types/api'

@Component({
  standalone: true,
  selector: 'button[server]',
  template: `
    <tui-icon icon="@tui.save" />
    <span tuiTitle>
      <strong>{{ server().hostname }}.local</strong>
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
  styles: ':host { width: stretch; border-radius: var(--tui-radius-l); }',
  hostDirectives: [TuiCell],
  imports: [DatePipe, TuiIcon, TuiTitle],
})
export class ServerComponent {
  private readonly dialogs = inject(TuiDialogService)

  readonly server = input.required<StartOSDiskInfo>()
}
