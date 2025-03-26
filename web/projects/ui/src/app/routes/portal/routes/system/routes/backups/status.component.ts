import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { TuiIcon } from '@taiga-ui/core'

@Component({
  standalone: true,
  selector: '[backupStatus]',
  template: `
    @if (type === 'create') {
      <tui-icon icon="@tui.cloud" class="g-positive" />
      Available for backup
    } @else {
      @if (backupStatus()) {
        <tui-icon icon="@tui.cloud-upload" class="g-positive" />
        StartOS backups detected
      } @else {
        <tui-icon icon="@tui.cloud-off" class="g-negative" />
        No StartOS backups
      }
    }
  `,
  styles: `
    :host {
      height: 2rem;
      display: flex;
      align-items: center;
      gap: 0.25rem;
    }

    tui-icon {
      font-size: 1rem;
    }

    :host-context(tui-root._mobile) {
      height: auto;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon],
})
export class BackupStatusComponent {
  readonly type = inject(ActivatedRoute).snapshot.data['type']
  readonly backupStatus = input(false)
}
