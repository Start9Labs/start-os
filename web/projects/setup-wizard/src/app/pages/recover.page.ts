import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { DriveComponent, ErrorService } from '@start9labs/shared'
import { TuiDialogService, TuiLoaderModule } from '@taiga-ui/core'
import {
  TuiButtonModule,
  TuiCardModule,
  TuiCellModule,
  TuiIconModule,
  TuiTitleModule,
} from '@taiga-ui/experimental'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { filter } from 'rxjs'
import { CifsComponent } from 'src/app/components/cifs.component'
import { PASSWORD } from 'src/app/components/password.component'
import {
  ApiService,
  CifsRecoverySource,
  DiskBackupTarget,
} from 'src/app/services/api.service'
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
          <tui-icon icon="tuiIconFolder" />
          <span tuiTitle>Open</span>
        </button>

        <h2>Physical Drive</h2>
        Restore StartOS data from a physical drive that is plugged directly into
        your server.
        <strong>
          Warning. Do not use this option if you are using a Raspberry Pi with
          an external SSD as your main data drive. The Raspberry Pi cannot not
          support more than one external drive without additional power and can
          cause data corruption.
        </strong>

        @for (d of drives; track d) {
          <button tuiCell [drive]="d" [disabled]="empty(d)" (click)="select(d)">
            <span tuiSubtitle>
              @if (empty(d)) {
                <tui-icon icon="tuiIconCloudOff" class="g-error" />
                <strong>No StartOS backup</strong>
              } @else {
                <tui-icon icon="tuiIconCloud" class="g-success" />
                <strong>StartOS backup detected</strong>
              }
            </span>
          </button>
        }

        <button tuiButton iconLeft="tuiIconRotateCwLarge" (click)="refresh()">
          Refresh
        </button>
      }
    </section>
  `,
  imports: [
    TuiCardModule,
    TuiLoaderModule,
    TuiButtonModule,
    TuiCellModule,
    TuiIconModule,
    TuiTitleModule,
    DriveComponent,
  ],
})
export default class RecoverPage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  loading = true
  drives: DiskBackupTarget[] = []

  async ngOnInit() {
    this.stateService.setupType = 'restore'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  empty(drive: DiskBackupTarget) {
    return !drive.startOs?.full
  }

  async getDrives() {
    this.drives = []
    try {
      await this.api.getDrives().then(disks =>
        disks
          .filter(d => d.partitions.length)
          .forEach(d => {
            d.partitions.forEach(p => {
              this.drives.push({ ...d, ...p })
            })
          }),
      )
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  select(target: DiskBackupTarget) {
    const { logicalname } = target

    if (!logicalname) return

    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Unlock Drive',
        size: 's',
        data: { target },
      })
      .pipe(filter(Boolean))
      .subscribe(password => {
        this.onSource(logicalname, password)
      })
  }

  onCifs() {
    this.dialogs
      .open<{
        cifs: CifsRecoverySource
        recoveryPassword: string
      }>(new PolymorpheusComponent(CifsComponent), {
        label: 'Connect Network Folder',
      })
      .subscribe(({ cifs, recoveryPassword }) => {
        this.stateService.recoverySource = { type: 'backup', target: cifs }
        this.stateService.recoveryPassword = recoveryPassword
        this.router.navigate(['storage'])
      })
  }

  private onSource(logicalname: string, password?: string) {
    this.stateService.recoverySource = {
      type: 'backup',
      target: { type: 'disk', logicalname },
    }
    this.stateService.recoveryPassword = password
    this.router.navigate(['storage'])
  }
}
