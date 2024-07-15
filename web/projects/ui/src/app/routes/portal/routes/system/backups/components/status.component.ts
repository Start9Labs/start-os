import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Emver } from '@start9labs/shared'
import { TuiIcon } from '@taiga-ui/core'
import { BackupTarget } from 'src/app/services/api/api.types'
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
  private readonly emver = inject(Emver)

  @Input({ required: true }) type!: BackupType
  @Input({ required: true }) target!: BackupTarget

  get status() {
    if (!this.target.mountable) {
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

    if (this.hasBackup) {
      return {
        icon: '@tui.cloud',
        color: 'var(--tui-text-positive)',
        text: 'Embassy backup detected',
      }
    }

    return {
      icon: '@tui.cloud-off',
      color: 'var(--tui-text-negative)',
      text: 'No Embassy backup',
    }
  }

  private get hasBackup(): boolean {
    return (
      !!this.target.startOs &&
      this.emver.compare(this.target.startOs.version, '0.3.0') !== -1
    )
  }
}
