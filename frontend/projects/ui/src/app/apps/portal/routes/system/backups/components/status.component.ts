import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { Emver } from '@start9labs/shared'
import { TuiSvgModule } from '@taiga-ui/core'
import { BackupTarget } from 'src/app/services/api/api.types'
import { BackupType } from '../types/backup-type'

@Component({
  selector: 'backups-status',
  template: `
    <tui-svg [src]="status.icon" [style.color]="status.color"></tui-svg>
    {{ status.text }}
  `,
  styles: [':host { display: flex; gap: 0.5rem; align-items: center }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiSvgModule],
})
export class BackupsStatusComponent {
  private readonly emver = inject(Emver)

  @Input({ required: true }) type!: BackupType
  @Input({ required: true }) target!: BackupTarget

  get status() {
    if (!this.target.mountable) {
      return {
        icon: 'tuiIconBarChartLarge',
        color: 'var(--tui-negative)',
        text: 'Unable to connect',
      }
    }

    if (this.type === 'create') {
      return {
        icon: 'tuiIconCloudLarge',
        color: 'var(--tui-positive)',
        text: this.hasBackup
          ? 'Available, contains existing backup'
          : 'Available for fresh backup',
      }
    }

    if (this.hasBackup) {
      return {
        icon: 'tuiIconCloudLarge',
        color: 'var(--tui-positive)',
        text: 'Embassy backup detected',
      }
    }

    return {
      icon: 'tuiIconCloudOffLarge',
      color: 'var(--tui-negative)',
      text: 'No Embassy backup',
    }
  }

  private get hasBackup(): boolean {
    return (
      !!this.target['embassy-os'] &&
      this.emver.compare(this.target['embassy-os'].version, '0.3.0') !== -1
    )
  }
}
