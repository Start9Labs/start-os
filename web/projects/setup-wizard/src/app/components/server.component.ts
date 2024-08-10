import { DatePipe } from '@angular/common'
import { Component, ElementRef, inject, input, Output } from '@angular/core'
import { StartOSDiskInfo } from '@start9labs/shared'
import { TuiDialogService, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { filter, fromEvent, switchMap } from 'rxjs'
import { PASSWORD } from 'src/app/components/password.component'

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

  @Output()
  readonly password = fromEvent(inject(ElementRef).nativeElement, 'click').pipe(
    switchMap(() =>
      this.dialogs.open<string>(PASSWORD, {
        label: 'Unlock Drive',
        size: 's',
        data: { passwordHash: this.server().passwordHash },
      }),
    ),
    filter(Boolean),
  )
}
