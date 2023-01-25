import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  inject,
} from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import { BackupColorPipe } from 'src/app/pipes/backup-color/backup-color.pipe'
import {
  ServerInfo,
  ServerStatusInfo,
} from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'widget-backups',
  templateUrl: './backups.component.html',
  styleUrls: ['./backups.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BackupsComponent {
  color = 'danger'
  iconPath = ''
  backupDetail = ''

  readonly data$ = inject(PatchDB)
    .watch$('server-info')
    .pipe(
      map((data: ServerInfo) => {
        const lastBackup = data['last-backup']
        if (lastBackup) this.getBackupDetails(lastBackup)
        this.iconPath = `/assets/img/svg/backup_icon_${this.color}.svg`
        return data
      }),
    )

  constructor(private backupColorPipe: BackupColorPipe) {}

  getBackupDetails(lastBackup: string) {
    const backupDate = new Date(lastBackup).getTime()
    const currentDate = new Date().getTime()
    const daysSinceLastBackup = Math.round(
      (currentDate - backupDate) / (1000 * 3600 * 24),
    )
    if (daysSinceLastBackup <= 0) {
      this.backupDetail = `Today at ${new Date(
        lastBackup,
      ).toLocaleTimeString()}`
    } else {
      this.backupDetail = `${daysSinceLastBackup} days ago`
    }
    this.color = this.backupColorPipe.transform(lastBackup)
  }
}
