import { TuiCell } from '@taiga-ui/layout'
import { TuiIcon, TuiTitle } from '@taiga-ui/core'
import { Component, Input } from '@angular/core'
import { RouterModule } from '@angular/router'

@Component({
  standalone: true,
  selector: 'app-recover',
  template: `
    <a tuiCell [routerLink]="disabled ? null : '/attach'">
      <tui-icon icon="@tui.box" />
      <span tuiTitle>
        <span class="g-success">Use Existing Drive</span>
        <span tuiSubtitle>
          Attach an existing StartOS data drive (not a backup)
        </span>
      </span>
    </a>
    <a tuiCell [routerLink]="disabled ? null : '/transfer'">
      <tui-icon icon="@tui.share" />
      <span tuiTitle>
        <span class="g-info">Transfer</span>
        <span tuiSubtitle>
          Transfer data from an existing StartOS data drive (not a backup) to a
          new, preferred drive
        </span>
      </span>
    </a>
    <a tuiCell [routerLink]="disabled ? null : '/recover'">
      <tui-icon icon="@tui.save" />
      <span tuiTitle>
        <span class="g-warning">Restore From Backup (Disaster Recovery)</span>
        <span tuiSubtitle>Restore StartOS data from an encrypted backup</span>
      </span>
    </a>
  `,
  imports: [RouterModule, TuiIcon, TuiCell, TuiTitle],
})
export class RecoverComponent {
  @Input() disabled = false
}
