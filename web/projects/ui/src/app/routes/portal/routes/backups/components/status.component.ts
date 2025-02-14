import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'
import { BackupType } from '../types/backup-type'

@Component({
  selector: 'backups-status',
  template: `
    <tui-icon [icon]="status.icon" [style.color]="status.color" />
    {{ status.text }}
  `,
  styles: [':host { display: flex; gap: 0.5rem; align-items: center }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiIcon],
})
export class BackupsStatusComponent {
  @Input({ required: true }) hasBackup!: boolean
  @Input({ required: true }) type!: BackupType
  @Input({ required: true }) mountable!: boolean

  get status() {
    if (!this.mountable) {
      return {
        icon: '@tui.bar-chart',
        color: 'var(--tui-text-negative)',
        text: 'Unable to connect',
      }
    }

    if (this.type === 'create') {
      return {
        icon: '@tui.cloud',
        color: 'var(--tui-text-positive)',
        text: this.hasBackup
          ? 'Available, contains existing backup'
          : 'Available for fresh backup',
      }
    }

    return this.hasBackup
      ? {
          icon: '@tui.cloud',
          color: 'var(--tui-text-positive)',
          text: 'Embassy backup detected',
        }
      : {
          icon: '@tui.cloud-off',
          color: 'var(--tui-text-negative)',
          text: 'No Embassy backup',
        }
  }
}
