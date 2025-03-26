import { AsyncPipe, DatePipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  OnInit,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { ActivatedRoute, RouterLink } from '@angular/router'
import { UnitConversionPipesModule } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiMapperPipe } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiLink,
  TuiLoader,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { PatchDB } from 'patch-db-client'
import {
  CifsBackupTarget,
  DiskBackupTarget,
} from 'src/app/services/api/api.types'
import { EOSService } from 'src/app/services/eos.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { BACKUP } from './backup.component'
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

    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ type === 'create' ? 'Create Backup' : 'Restore Backup' }}</h3>
        <p tuiSubtitle>
          @if (type === 'create') {
            Back up StartOS and service data by connecting to a device on your
            local network or a physical drive connected to your server.
            <a
              tuiLink
              href="https://docs.start9.com/0.3.5.x/user-manual/backups/backup-create"
              target="_blank"
              rel="noreferrer"
              appearance="action-grayscale"
              iconEnd="@tui.external-link"
              [pseudo]="true"
              [textContent]="'View instructions'"
            ></a>
          } @else {
            Restore StartOS and service data from a device on your local network
            or a physical drive connected to your server that contains an
            existing backup.
            <a
              tuiLink
              href="https://docs.start9.com/0.3.5.x/user-manual/backups/backup-restore"
              target="_blank"
              rel="noreferrer"
              appearance="action-grayscale"
              iconEnd="@tui.external-link"
              [pseudo]="true"
              [textContent]="'View instructions'"
            ></a>
          }
        </p>
      </hgroup>
    </header>

    @if (type === 'create' && server(); as s) {
      <tui-notification [appearance]="s.lastBackup | tuiMapper: toAppearance">
        <div tuiTitle>
          Last Backup
          <div tuiSubtitle>
            {{ s.lastBackup ? (s.lastBackup | date: 'medium') : 'never' }}
          </div>
        </div>
      </tui-notification>
    }

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
          A folder on another computer that is connected to the same network as
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
          A physical drive that is plugged directly into your Start9 Server.
        </section>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    AsyncPipe,
    DatePipe,
    RouterLink,
    TuiButton,
    TuiLoader,
    TuiLink,
    TuiHeader,
    TuiTitle,
    TuiNotification,
    TuiMapperPipe,
    TitleDirective,
    UnitConversionPipesModule,
    BackupNetworkComponent,
    BackupPhysicalComponent,
    BackupProgressComponent,
  ],
})
export default class SystemBackupComponent implements OnInit {
  readonly dialogs = inject(TuiResponsiveDialogService)
  readonly type = inject(ActivatedRoute).snapshot.data['type']
  readonly service = inject(BackupService)
  readonly eos = inject(EOSService)
  readonly server = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('serverInfo'),
  )

  readonly toAppearance = (lastBackup: string | null) => {
    if (!lastBackup) return 'negative'

    const currentDate = new Date().valueOf()
    const backupDate = new Date(lastBackup).valueOf()
    const diff = currentDate - backupDate
    const week = 604800000

    if (diff <= week) {
      return 'positive'
    } else if (diff > week && diff <= week * 2) {
      return 'warning'
    } else {
      return 'negative'
    }
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
