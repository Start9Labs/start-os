import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDataList,
  TuiDialogService,
  TuiDropdown,
  TuiIcon,
  TuiLoader,
  TuiOptGroup,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'
import { StartOSDiskInfoFull, StartOSDiskInfoWithId } from '../types'
import { CIFS, CifsResult } from '../components/cifs.component'
import { SELECT_NETWORK_BACKUP } from '../components/select-network-backup.dialog'
import { UnlockPasswordDialog } from '../components/unlock-password.dialog'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>
          Select Backup
          <span tuiSubtitle>
            Select the StartOS backup you want to restore
            <a class="refresh" (click)="refresh()">
              <tui-icon icon="@tui.rotate-cw" />
              Refresh
            </a>
          </span>
        </h2>
      </header>

      @if (loading) {
        <tui-loader />
      } @else {
        <button
          tuiButton
          iconEnd="@tui.chevron-down"
          [tuiDropdown]="dropdown"
          [tuiDropdownLimitWidth]="'fixed'"
          [(tuiDropdownOpen)]="open"
          style="width: 100%"
        >
          Select Backup
        </button>

        <ng-template #dropdown>
          <tui-data-list>
            <tui-opt-group>
              <button tuiOption new (click)="openCifs()">
                <tui-icon icon="@tui.folder-plus" />
                Open Network Backup
              </button>
            </tui-opt-group>
            <tui-opt-group label="Physical Backups">
              @for (server of physicalServers; track server.id) {
                <button tuiOption new (click)="selectPhysicalBackup(server)">
                  <div class="server-item">
                    <span>{{ server.id }}</span>
                    <small>
                      {{ server.drive.vendor }} {{ server.drive.model }} ·
                      {{ server.partition.logicalname }}
                    </small>
                  </div>
                </button>
              } @empty {
                <div class="no-items">No physical backups</div>
              }
            </tui-opt-group>
          </tui-data-list>
        </ng-template>
      }
    </section>
  `,
  styles: `
    .refresh {
      display: inline-flex;
      align-items: center;
      gap: 0.25rem;
      cursor: pointer;
      color: var(--tui-text-action);

      tui-icon {
        font-size: 0.875rem;
      }
    }

    .server-item {
      display: flex;
      flex-direction: column;

      small {
        opacity: 0.7;
      }
    }

    .no-items {
      padding: 0.5rem 0.75rem;
      color: var(--tui-text-secondary);
      font-style: italic;
    }
  `,
  imports: [
    TuiButton,
    TuiCardLarge,
    TuiDataList,
    TuiDropdown,
    TuiLoader,
    TuiIcon,
    TuiOptGroup,
    TuiTitle,
    TuiHeader,
  ],
})
export default class RestorePage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  loading = true
  open = false
  physicalServers: StartOSDiskInfoFull[] = []

  async ngOnInit() {
    await this.loadDrives()
  }

  async refresh() {
    this.loading = true
    await this.loadDrives()
  }

  openCifs() {
    this.open = false
    this.dialogs
      .open<CifsResult>(CIFS, {
        label: 'Connect Network Folder',
        size: 's',
      })
      .subscribe(result => {
        if (result) {
          this.handleCifsResult(result)
        }
      })
  }

  selectPhysicalBackup(server: StartOSDiskInfoFull) {
    this.open = false
    this.showUnlockDialog(server.id, {
      type: 'disk',
      logicalname: server.partition.logicalname,
    })
  }

  private handleCifsResult(result: CifsResult) {
    if (result.servers.length === 1) {
      this.showUnlockDialog(result.servers[0]!.id, {
        type: 'cifs',
        ...result.cifs,
      })
    } else if (result.servers.length > 1) {
      this.showSelectNetworkBackupDialog(result.cifs, result.servers)
    }
  }

  private showSelectNetworkBackupDialog(
    cifs: T.Cifs,
    servers: StartOSDiskInfoWithId[],
  ) {
    this.dialogs
      .open<StartOSDiskInfoWithId | null>(SELECT_NETWORK_BACKUP, {
        label: 'Select Network Backup',
        size: 's',
        data: { servers },
      })
      .subscribe(server => {
        if (server) {
          this.showUnlockDialog(server.id, { type: 'cifs', ...cifs })
        }
      })
  }

  private showUnlockDialog(
    serverId: string,
    target: { type: 'disk'; logicalname: string } | ({ type: 'cifs' } & T.Cifs),
  ) {
    this.dialogs
      .open<string | null>(new PolymorpheusComponent(UnlockPasswordDialog), {
        label: 'Unlock Backup',
        size: 's',
      })
      .subscribe(password => {
        if (password) {
          this.stateService.recoverySource = {
            type: 'backup',
            target,
            serverId,
            password,
          }
          this.router.navigate(['/password'])
        }
      })
  }

  private async loadDrives() {
    this.physicalServers = []

    try {
      const drives = await this.api.getDisks()

      this.physicalServers = drives.flatMap(drive =>
        drive.partitions.flatMap(partition =>
          Object.entries(partition.startOs).map(([id, val]) => ({
            id,
            ...val,
            partition,
            drive,
          })),
        ),
      )
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }
}
