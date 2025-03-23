import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
} from '@angular/core'
import { ActivatedRoute, RouterLink } from '@angular/router'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton, TuiLink, TuiLoader } from '@taiga-ui/core'
import { BACKUP } from 'src/app/routes/portal/routes/system/routes/backups/backup.component'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { EOSService } from 'src/app/services/eos.service'
import { TitleDirective } from 'src/app/services/title.service'
import { BackupService, MappedBackupTarget } from './backup.service'
import { BackupNetworkComponent } from './network.component'
import { BackupPhysicalComponent } from './physical.component'
import { BackupProgressComponent } from './progress.component'
import { BACKUP_RESTORE } from './restore.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      {{ type === 'create' ? 'Create Backup' : 'Restore Backup' }}
    </ng-container>

    @if (type === 'create' && (eos.backingUp$ | async)) {
      <section backupProgress></section>
    } @else {
      @if (service.loading()) {
        <tui-loader
          textContent="Fetching backups"
          size="l"
          [style.height.rem]="20"
        />
      } @else {
        <section (networkFolders)="onTarget($event)">
          {{ text }}
          a folder on another computer that is connected to the same network as
          your Start9 server. View the
          <a
            tuiLink
            href="https://docs.start9.com/0.3.5.x/user-manual/backups/backup-create#network-folder"
            target="_blank"
            rel="noreferrer"
            iconEnd="@tui.external-link"
            [textContent]="'Instructions'"
          ></a>
        </section>
        <section (physicalFolders)="onTarget($event)">
          {{ text }}
          a physical drive that is plugged directly into your Start9 Server.
          View the
          <a
            tuiLink
            href="https://docs.start9.com/0.3.5.x/user-manual/backups/backup-create#physical-drive"
            target="_blank"
            rel="noreferrer"
            iconEnd="@tui.external-link"
            [textContent]="'Instructions'"
          ></a>
        </section>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    RouterLink,
    TuiButton,
    TuiLoader,
    TuiLink,
    TitleDirective,
    UnitConversionPipesModule,
    BackupNetworkComponent,
    BackupPhysicalComponent,
    AsyncPipe,
    BackupProgressComponent,
  ],
})
export default class SystemBackupComponent implements OnInit {
  readonly dialogs = inject(TuiResponsiveDialogService)
  readonly type = inject(ActivatedRoute).snapshot.data['type']
  readonly service = inject(BackupService)
  readonly eos = inject(EOSService)

  get text() {
    return this.type === 'create'
      ? 'Backup server to'
      : 'Restore your services from'
  }

  ngOnInit() {
    this.service.getBackupTargets()
  }

  onTarget(target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>) {
    const component = this.type === 'create' ? BACKUP : BACKUP_RESTORE
    const label =
      this.type === 'create'
        ? 'Select Services to Back Up'
        : 'Select server backup'

    this.dialogs.open(component, { label, data: target }).subscribe()
  }
}
