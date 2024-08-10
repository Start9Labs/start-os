import { DatePipe } from '@angular/common'
import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogService,
  TuiIcon,
  TuiLoader,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { CIFS, CifsResponse } from 'src/app/components/cifs.component'
import { ServerComponent } from 'src/app/components/server.component'
import { ApiService, StartOSDiskInfoFull } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <section tuiCardLarge>
      <header>Restore from Backup</header>
      @if (loading) {
        <tui-loader />
      } @else {
        <h2>Network Folder</h2>
        Restore StartOS data from a folder on another computer that is connected
        to the same network as your server.

        <button tuiCell (click)="onCifs()">
          <tui-icon icon="@tui.folder" />
          <span tuiTitle>Open</span>
        </button>

        <h2>Physical Drive</h2>
        <div>
          Restore StartOS data from a physical drive that is plugged directly
          into your server.
        </div>
        <strong>
          Warning. Do not use this option if you are using a Raspberry Pi with
          an external SSD as your main data drive. The Raspberry Pi cannot not
          support more than one external drive without additional power and can
          cause data corruption.
        </strong>

        @for (server of servers; track $index) {
          <button
            [server]="server"
            (password)="select($event, server)"
          ></button>
        }

        <button tuiButton iconStart="@tui.rotate-cw" (click)="refresh()">
          Refresh
        </button>
      }
    </section>
  `,
  imports: [
    TuiCardLarge,
    TuiLoader,
    TuiButton,
    TuiCell,
    TuiIcon,
    TuiTitle,
    DatePipe,
    ServerComponent,
  ],
})
export default class RecoverPage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  loading = true
  servers: StartOSDiskInfoFull[] = []

  async ngOnInit() {
    this.stateService.setupType = 'restore'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    this.servers = []

    try {
      const drives = await this.api.getDrives()

      this.servers = drives.flatMap(drive =>
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

  select(password: string, server: StartOSDiskInfoFull) {
    this.stateService.recoverySource = {
      type: 'backup',
      target: {
        type: 'disk',
        logicalname: server.partition.logicalname,
      },
      serverId: server.id,
      password,
    }
    this.router.navigate(['storage'])
  }

  onCifs() {
    this.dialogs
      .open<CifsResponse>(CIFS, {
        label: 'Connect Network Folder',
      })
      .subscribe(({ cifs, serverId, password }) => {
        this.stateService.recoverySource = {
          type: 'backup',
          target: {
            type: 'cifs',
            ...cifs,
          },
          serverId,
          password,
        }
        this.router.navigate(['storage'])
      })
  }
}
