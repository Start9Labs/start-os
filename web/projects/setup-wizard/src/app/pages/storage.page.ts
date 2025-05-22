import { Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  DiskInfo,
  DriveComponent,
  ErrorService,
  i18nKey,
  LoadingService,
  toGuid,
} from '@start9labs/shared'
import { TuiButton, TuiDialogService, TuiLoader } from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { filter, of, switchMap } from 'rxjs'
import { PASSWORD } from 'src/app/components/password.component'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <section tuiCardLarge="compact">
      @if (loading || drives.length) {
        <header>Select storage drive</header>
        This is the drive where your StartOS data will be stored.
      } @else {
        <header>No drives found</header>
        Please connect a storage drive to your server. Then click "Refresh".
      }

      @if (loading) {
        <tui-loader />
      }

      @for (d of drives; track d) {
        <button tuiCell [drive]="d" [disabled]="isSmall(d)" (click)="select(d)">
          @if (isSmall(d)) {
            <span tuiSubtitle class="g-negative">Drive capacity too small</span>
          }
        </button>
      }
      <footer>
        <button tuiButton iconStart="@tui.rotate-cw" (click)="refresh()">
          Refresh
        </button>
      </footer>
    </section>
  `,
  imports: [TuiCardLarge, TuiLoader, TuiCell, TuiButton, DriveComponent],
})
export default class StoragePage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(TuiDialogService)
  private readonly stateService = inject(StateService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)

  drives: DiskInfo[] = []
  loading = true

  async ngOnInit() {
    await this.getDrives()
  }

  isSmall({ capacity }: DiskInfo) {
    return capacity < 34359738368
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    this.loading = true
    try {
      const disks = await this.api.getDrives()
      if (this.stateService.setupType === 'fresh') {
        this.drives = disks
      } else if (
        this.stateService.setupType === 'restore' &&
        this.stateService.recoverySource?.type === 'backup'
      ) {
        if (this.stateService.recoverySource.target.type === 'disk') {
          const logicalname =
            this.stateService.recoverySource.target.logicalname
          this.drives = disks.filter(
            d => !d.partitions.map(p => p.logicalname).includes(logicalname),
          )
        } else {
          this.drives = disks
        }
      } else if (
        this.stateService.setupType === 'transfer' &&
        this.stateService.recoverySource?.type === 'migrate'
      ) {
        const guid = this.stateService.recoverySource.guid
        this.drives = disks.filter(d => {
          return (
            d.guid !== guid && !d.partitions.map(p => p.guid).includes(guid)
          )
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  select(drive: DiskInfo) {
    of(!toGuid(drive) && !drive.partitions.some(p => p.used))
      .pipe(
        switchMap(unused =>
          unused
            ? of(true)
            : this.dialogs.open(TUI_CONFIRM, {
                label: 'Warning',
                size: 's',
                data: {
                  content:
                    '<strong>Drive contains data!</strong><p>All data stored on this drive will be permanently deleted.</p>',
                  yes: 'Continue',
                  no: 'Cancel',
                },
              }),
        ),
      )
      .pipe(filter(Boolean))
      .subscribe(() => {
        // for backup recoveries
        if (this.stateService.recoverySource?.type === 'backup') {
          this.setupEmbassy(
            drive.logicalname,
            this.stateService.recoverySource.password,
          )
        } else {
          // for migrations and fresh setups
          this.promptPassword(drive.logicalname)
        }
      })
  }

  private promptPassword(logicalname: string) {
    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Set Password',
        size: 's',
        data: { storageDrive: true },
      })
      .subscribe(password => {
        this.setupEmbassy(logicalname, password)
      })
  }

  private async setupEmbassy(
    logicalname: string,
    password: string,
  ): Promise<void> {
    const loader = this.loader
      .open('Connecting to drive' as i18nKey)
      .subscribe()

    try {
      await this.stateService.setupEmbassy(logicalname, password)
      await this.router.navigate(['loading'])
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
