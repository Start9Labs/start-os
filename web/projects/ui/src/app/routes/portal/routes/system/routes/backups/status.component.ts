import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { i18nPipe } from '@start9labs/shared'
import { TuiIcon } from '@taiga-ui/core'

@Component({
  selector: '[backupStatus]',
  template: `
    @if (type === 'create') {
      <tui-icon
        [icon]="physical() ? '@tui.hard-drive' : '@tui.signal-high'"
        class="g-positive"
      />
      {{ 'Available for backup' | i18n }}
    } @else {
      @if (backupStatus()) {
        <tui-icon icon="@tui.save" class="g-positive" />
        {{ 'StartOS backups detected' | i18n }}
      } @else {
        <tui-icon icon="@tui.file-x" class="g-negative" />
        {{ 'No StartOS backups detected' | i18n }}
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
      min-width: 1.25rem;
      text-align: center;
    }

    :host-context(tui-root._mobile) {
      height: auto;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon, i18nPipe],
})
export class BackupStatusComponent {
  readonly type = inject(ActivatedRoute).snapshot.data['type']
  readonly backupStatus = input(false)
  readonly physical = input(false)
}
