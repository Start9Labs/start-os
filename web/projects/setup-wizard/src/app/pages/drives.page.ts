import {
  ChangeDetectorRef,
  Component,
  HostListener,
  inject,
} from '@angular/core'
import { Router } from '@angular/router'
import { FormsModule } from '@angular/forms'
import {
  DialogService,
  DiskInfo,
  ErrorService,
  i18nKey,
  i18nPipe,
  LoadingService,
  toGuid,
} from '@start9labs/shared'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiIcon,
  TuiLoader,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiDataListWrapper, TuiSelect, TuiTooltip } from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { filter, Subscription } from 'rxjs'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'
import { PreserveOverwriteDialog } from '../components/preserve-overwrite.dialog'

@Component({
  template: `
    @if (!shuttingDown) {
      <section tuiCardLarge="compact">
        <header tuiHeader>
          <h2 tuiTitle>{{ 'Select Drives' | i18n }}</h2>
        </header>

        @if (loading) {
          <tui-loader />
        } @else if (drives.length === 0) {
          <p class="no-drives">
            {{
              'No drives found. Please connect a drive and click Refresh.'
                | i18n
            }}
          </p>
        } @else {
          <tui-textfield
            [stringify]="stringify"
            [disabledItemHandler]="osDisabled"
          >
            <label tuiLabel>{{ 'OS Drive' | i18n }}</label>
            @if (mobile) {
              <select
                tuiSelect
                [ngModel]="selectedOsDrive"
                (ngModelChange)="onOsDriveChange($event)"
                [items]="drives"
              ></select>
            } @else {
              <input
                tuiSelect
                [ngModel]="selectedOsDrive"
                (ngModelChange)="onOsDriveChange($event)"
              />
            }
            @if (!mobile) {
              <tui-data-list-wrapper
                new
                *tuiTextfieldDropdown
                [items]="drives"
                [itemContent]="driveContent"
              />
            }
            <tui-icon [tuiTooltip]="osDriveTooltip" />
          </tui-textfield>

          <tui-textfield
            [stringify]="stringify"
            [disabledItemHandler]="dataDisabled"
          >
            <label tuiLabel>{{ 'Data Drive' | i18n }}</label>
            @if (mobile) {
              <select
                tuiSelect
                [(ngModel)]="selectedDataDrive"
                (ngModelChange)="onDataDriveChange($event)"
                [items]="drives"
              ></select>
            } @else {
              <input
                tuiSelect
                [(ngModel)]="selectedDataDrive"
                (ngModelChange)="onDataDriveChange($event)"
              />
            }
            @if (!mobile) {
              <tui-data-list-wrapper
                new
                *tuiTextfieldDropdown
                [items]="drives"
                [itemContent]="driveContent"
              />
            }
            @if (preserveData === true) {
              <tui-icon
                icon="@tui.database"
                style="color: var(--tui-status-positive); pointer-events: none"
              />
            }
            @if (preserveData === false) {
              <tui-icon
                icon="@tui.database-zap"
                style="color: var(--tui-status-negative); pointer-events: none"
              />
            }
            <tui-icon [tuiTooltip]="dataDriveTooltip" />
          </tui-textfield>

          <ng-template #driveContent let-drive>
            <div class="drive-item">
              <span class="drive-name">
                {{ driveName(drive) }}
              </span>
              <small>
                {{ formatCapacity(drive.capacity) }} · {{ drive.logicalname }}
              </small>
            </div>
          </ng-template>
        }

        <footer>
          @if (drives.length === 0) {
            <button tuiButton appearance="secondary" (click)="refresh()">
              {{ 'Refresh' | i18n }}
            </button>
          } @else {
            <button
              tuiButton
              [disabled]="!selectedOsDrive || !selectedDataDrive"
              (click)="continue()"
            >
              {{ 'Continue' | i18n }}
            </button>
          }
        </footer>
      </section>
    }
  `,
  styles: `
    .no-drives {
      text-align: center;
      color: var(--tui-text-secondary);
      padding: 2rem;
    }

    .drive-item {
      display: flex;
      flex-direction: column;

      small {
        opacity: 0.7;
      }
    }
  `,
  imports: [
    FormsModule,
    TuiCardLarge,
    TuiButton,
    TuiIcon,
    TuiLoader,
    TuiTextfield,
    TuiSelect,
    TuiDataListWrapper,
    TuiTooltip,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class DrivesPage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(DialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)
  private readonly cdr = inject(ChangeDetectorRef)
  private readonly i18n = inject(i18nPipe)

  protected readonly mobile = inject(TUI_IS_MOBILE)

  @HostListener('document:keydown', ['$event'])
  onKeydown(event: KeyboardEvent) {
    if (event.ctrlKey && event.shiftKey && event.key === 'X') {
      event.preventDefault()
      this.shutdownServer()
    }
  }

  readonly osDriveTooltip = this.i18n.transform(
    'The drive where the StartOS operating system will be installed.',
  )
  readonly dataDriveTooltip = this.i18n.transform(
    'The drive where your StartOS data (services, settings, etc.) will be stored. This can be the same as the OS drive or a separate drive.',
  )

  private readonly MIN_OS = 18 * 2 ** 30 // 18 GiB
  private readonly MIN_DATA = 20 * 2 ** 30 // 20 GiB
  private readonly MIN_BOTH = 38 * 2 ** 30 // 38 GiB

  drives: DiskInfo[] = []
  loading = true
  shuttingDown = false
  private dialogSub?: Subscription
  selectedOsDrive: DiskInfo | null = null
  selectedDataDrive: DiskInfo | null = null
  preserveData: boolean | null = null

  readonly osDisabled = (drive: DiskInfo): boolean =>
    drive.capacity < this.MIN_OS

  dataDisabled = (drive: DiskInfo): boolean => drive.capacity < this.MIN_DATA

  readonly driveName = (drive: DiskInfo): string =>
    [drive.vendor, drive.model].filter(Boolean).join(' ') ||
    this.i18n.transform('Unknown Drive')

  readonly stringify = (drive: DiskInfo | null) =>
    drive ? this.driveName(drive) : ''

  formatCapacity(bytes: number): string {
    const gb = bytes / 1e9
    if (gb >= 1000) {
      return `${(gb / 1000).toFixed(1)} TB`
    }
    return `${gb.toFixed(0)} GB`
  }

  async ngOnInit() {
    await this.loadDrives()
  }

  async refresh() {
    this.loading = true
    this.selectedOsDrive = null
    this.selectedDataDrive = null
    this.preserveData = null
    await this.loadDrives()
  }

  onOsDriveChange(osDrive: DiskInfo | null) {
    this.selectedOsDrive = osDrive
    this.dataDisabled = (drive: DiskInfo) => {
      if (osDrive && drive.logicalname === osDrive.logicalname) {
        return drive.capacity < this.MIN_BOTH
      }
      return drive.capacity < this.MIN_DATA
    }

    // Clear data drive if it's now invalid
    if (this.selectedDataDrive && this.dataDisabled(this.selectedDataDrive)) {
      this.selectedDataDrive = null
      this.preserveData = null
    }
  }

  onDataDriveChange(drive: DiskInfo | null) {
    this.preserveData = null

    if (!drive) {
      return
    }

    const hasStartOSData = !!toGuid(drive)
    if (hasStartOSData) {
      this.showPreserveOverwriteDialog()
    }
  }

  continue() {
    if (!this.selectedOsDrive || !this.selectedDataDrive) return

    const sameDevice =
      this.selectedOsDrive.logicalname === this.selectedDataDrive.logicalname
    const dataHasStartOS = !!toGuid(this.selectedDataDrive)

    // Scenario 1: Same drive, has StartOS data, preserving → no warning
    if (sameDevice && dataHasStartOS && this.preserveData) {
      this.installOs(false)
      return
    }

    // Scenario 2: Different drives, preserving data → warn OS only
    if (!sameDevice && this.preserveData) {
      this.showOsDriveWarning()
      return
    }

    // Scenario 3: All other cases → warn about overwriting
    this.showFullWarning(sameDevice)
  }

  private showPreserveOverwriteDialog() {
    let selectionMade = false

    this.dialogs
      .openComponent<boolean>(
        new PolymorpheusComponent(PreserveOverwriteDialog),
        {
          label: 'StartOS Data Detected',
          size: 's',
          dismissible: true,
          closeable: true,
        },
      )
      .subscribe({
        next: preserve => {
          selectionMade = true
          this.preserveData = preserve
          this.cdr.markForCheck()
        },
        complete: () => {
          if (!selectionMade) {
            // Dialog was dismissed without selection - clear the data drive
            this.selectedDataDrive = null
            this.preserveData = null
            this.cdr.markForCheck()
          }
        },
      })
  }

  private showOsDriveWarning() {
    this.dialogs
      .openConfirm({
        label: 'Warning',
        size: 's',
        data: {
          content: `<ul>
<li class="g-negative">${this.i18n.transform('Data on the OS drive may be overwritten.')}</li>
<li class="g-positive">${this.i18n.transform('your StartOS data on the data drive will be preserved.')}</li>
</ul>` as i18nKey,
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.installOs(false)
      })
  }

  private showFullWarning(sameDevice: boolean) {
    const message = sameDevice
      ? `<p class="g-negative">${this.i18n.transform('Data on this drive will be overwritten.')}</p>`
      : `<p class="g-negative">${this.i18n.transform('Data on both drives will be overwritten.')}</p>`

    this.dialogs
      .openConfirm({
        label: 'Warning',
        size: 's',
        data: {
          content: message as i18nKey,
          yes: 'Continue',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => {
        this.installOs(true)
      })
  }

  private async installOs(wipe: boolean) {
    const loader = this.loader.open('Installing StartOS').subscribe()

    try {
      const result = await this.api.installOs({
        osDrive: this.selectedOsDrive!.logicalname,
        dataDrive: {
          logicalname: this.selectedDataDrive!.logicalname,
          wipe,
        },
      })

      this.stateService.dataDriveGuid = result.guid
      this.stateService.attach = result.attach

      loader.unsubscribe()

      // Show success dialog
      this.dialogSub = this.dialogs
        .openAlert('StartOS has been installed successfully.', {
          label: 'Installation Complete!',
          size: 's',
          dismissible: false,
          closeable: true,
          data: { button: this.i18n.transform('Continue to Setup') },
        })
        .subscribe({
          complete: () => {
            this.navigateToNextStep(result.attach)
          },
        })
    } catch (e: any) {
      loader.unsubscribe()
      this.errorService.handleError(e)
    }
  }

  private async navigateToNextStep(attach: boolean) {
    if (attach) {
      this.stateService.setupType = 'attach'
      await this.router.navigate(['/password'])
    } else {
      await this.router.navigate(['/home'])
    }
  }

  private async shutdownServer() {
    this.dialogSub?.unsubscribe()
    const loader = this.loader.open('Beginning shutdown').subscribe()

    try {
      await this.api.shutdown()
      this.shuttingDown = true
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async loadDrives() {
    try {
      this.drives = (await this.api.getDisks()).filter(d => d.capacity > 0)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }
}
