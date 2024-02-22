import { Component, Input } from '@angular/core'
import { RouterLink } from '@angular/router'
import {
  TuiCellModule,
  TuiIconModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'

@Component({
  standalone: true,
  selector: 'app-recover',
  template: `
    <a tuiCell [routerLink]="disabled ? null : '/attach'">
      <tui-icon icon="tuiIconBoxLarge" />
      <span tuiTitle>
        <span class="g-success">Use Existing Drive</span>
        <span tuiSubtitle>
          Attach an existing StartOS data drive (not a backup)
        </span>
      </span>
    </a>
    <a tuiCell [routerLink]="disabled ? null : '/transfer'">
      <tui-icon icon="tuiIconShareLarge" />
      <span tuiTitle>
        <span class="g-info">Transfer</span>
        <span tuiSubtitle>
          Transfer data from an existing StartOS data drive (not a backup) to a
          new, preferred drive
        </span>
      </span>
    </a>
    <a tuiCell [routerLink]="disabled ? null : '/recover'">
      <tui-icon icon="tuiIconSave" />
      <span tuiTitle>
        <span class="g-warning">Restore From Backup (Disaster Recovery)</span>
        <span tuiSubtitle>Restore StartOS data from an encrypted backup</span>
      </span>
    </a>
  `,
  imports: [RouterLink, TuiIconModule, TuiCellModule, TuiTitleModule],
})
export class RecoverComponent {
  @Input() disabled = false
}
