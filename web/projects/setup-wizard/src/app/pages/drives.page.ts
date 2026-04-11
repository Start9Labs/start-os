import {
  ChangeDetectorRef,
  Component,
  HostListener,
  inject,
} from '@angular/core'
import {
  AbstractControl,
  FormControl,
  FormGroup,
  ReactiveFormsModule,
  ValidatorFn,
  Validators,
} from '@angular/forms'
import { Router } from '@angular/router'
import { WA_IS_MOBILE } from '@ng-web-apis/platform'
import {
  DialogService,
  DiskInfo,
  ErrorService,
  i18nKey,
  i18nPipe,
  toGuid,
} from '@start9labs/shared'
import { TuiMapperPipe, TuiValidator } from '@taiga-ui/cdk'
import {
  TuiButton,
  TuiError,
  TuiIcon,
  TuiLoader,
  TuiNotification,
  TUI_VALIDATION_ERRORS,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiDataListWrapper,
  TuiNotificationMiddleService,
  TuiSelect,
  TuiTooltip,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiForm, TuiHeader } from '@taiga-ui/layout'
import { distinctUntilChanged, filter, Subscription } from 'rxjs'
import { PRESERVE_OVERWRITE } from '../components/preserve-overwrite.dialog'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

@Component({
  template: `
    @if (!shuttingDown) {
      @if (loading) {
        <section tuiCardLarge="compact">
          <header tuiHeader>
            <h2 tuiTitle>{{ 'Select Drives' | i18n }}</h2>
          </header>
          <tui-loader />
        </section>
      } @else if (drives.length === 0) {
        <section tuiCardLarge="compact">
          <header tuiHeader>
            <h2 tuiTitle>{{ 'Select Drives' | i18n }}</h2>
          </header>
          <p tuiNotification size="m" appearance="warning">
            {{
              'No drives found. Please connect a drive and click Refresh.'
                | i18n
            }}
          </p>
          <footer>
            <button tuiButton appearance="secondary" (click)="refresh()">
              {{ 'Refresh' | i18n }}
            </button>
          </footer>
        </section>
      } @else {
        <form tuiCardLarge="compact" tuiForm [formGroup]="form">
          <header tuiHeader>
            <h2 tuiTitle>{{ 'Select Drives' | i18n }}</h2>
          </header>

          <tui-textfield [stringify]="stringify">
            <label tuiLabel>{{ 'OS Drive' | i18n }}</label>
            @if (mobile) {
              <select
                tuiSelect
                formControlName="osDrive"
                [items]="drives"
              ></select>
            } @else {
              <input tuiSelect formControlName="osDrive" />
            }
            @if (!mobile) {
              <tui-data-list-wrapper
                *tuiDropdown
                [items]="drives"
                [itemContent]="driveContent"
              />
            }
            <tui-icon [tuiTooltip]="osDriveTooltip" />
          </tui-textfield>
          @if (form.controls.osDrive.touched && form.controls.osDrive.invalid) {
            <tui-error formControlName="osDrive" />
          }

          <tui-textfield [stringify]="stringify">
            <label tuiLabel>{{ 'Data Drive' | i18n }}</label>
            @if (mobile) {
              <select
                tuiSelect
                formControlName="dataDrive"
                [items]="drives"
                [tuiValidator]="
                  form.controls.osDrive.value | tuiMapper: dataValidator
                "
              ></select>
            } @else {
              <input
                tuiSelect
                formControlName="dataDrive"
                [tuiValidator]="
                  form.controls.osDrive.value | tuiMapper: dataValidator
                "
              />
            }
            @if (!mobile) {
              <tui-data-list-wrapper
                *tuiDropdown
                [items]="drives"
                [itemContent]="driveContent"
              />
            }
            @if (preserveData === true) {
              <tui-icon icon="@tui.database" class="g-positive" />
            }
            @if (preserveData === false) {
              <tui-icon icon="@tui.database-zap" class="g-negative" />
            }
            <tui-icon [tuiTooltip]="dataDriveTooltip" />
          </tui-textfield>
          @if (
            form.controls.dataDrive.touched && form.controls.dataDrive.invalid
          ) {
            <tui-error formControlName="dataDrive" />
          }

          <ng-template #driveContent let-drive>
            <span tuiTitle>
              {{ driveName(drive) }}
              <span tuiSubtitle>
                {{ formatCapacity(drive.capacity) }} · {{ drive.logicalname }}
              </span>
            </span>
          </ng-template>

          <footer>
            <button tuiButton [disabled]="form.invalid" (click)="continue()">
              {{ 'Continue' | i18n }}
            </button>
          </footer>
        </form>
      }
    }
  `,
  styles: `
    tui-icon:not([tuiTooltip]) {
      pointer-events: none;
    }
  `,
  imports: [
    ReactiveFormsModule,
    TuiCardLarge,
    TuiForm,
    TuiButton,
    TuiError,
    TuiIcon,
    TuiLoader,
    TuiNotification,
    TuiSelect,
    TuiDataListWrapper,
    TuiTooltip,
    TuiValidator,
    TuiMapperPipe,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
  providers: [
    {
      provide: TUI_VALIDATION_ERRORS,
      useFactory: () => {
        const i18n = inject(i18nPipe)
        return {
          required: i18n.transform('Required'),
        }
      },
    },
  ],
})
export default class DrivesPage {
  private readonly api = inject(ApiService)
  private readonly router = inject(Router)
  private readonly dialogs = inject(DialogService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)
  private readonly cdr = inject(ChangeDetectorRef)
  private readonly i18n = inject(i18nPipe)

  protected readonly mobile = inject(WA_IS_MOBILE)

  @HostListener('document:keydown', ['$event'])
  onKeydown(event: KeyboardEvent) {
    if (event.ctrlKey && event.shiftKey && event.key === 'X') {
      event.preventDefault()
      this.shutdownServer()
    }
  }

  readonly osDriveTooltip = this.i18n.transform(
    'The drive where the StartOS operating system will be installed. Minimum 18 GB.',
  )
  readonly dataDriveTooltip = this.i18n.transform(
    'The drive where your StartOS data (services, settings, etc.) will be stored. This can be the same as the OS drive or a separate drive. Minimum 20 GB, or 38 GB if using a single drive for both OS and data.',
  )

  private readonly MIN_OS = 18 * 2 ** 30 // 18 GiB
  private readonly MIN_DATA = 20 * 2 ** 30 // 20 GiB
  private readonly MIN_BOTH = 38 * 2 ** 30 // 38 GiB

  private readonly osCapacityValidator: ValidatorFn = ({
    value,
  }: AbstractControl) => {
    if (!value) return null
    return value.capacity < this.MIN_OS
      ? {
          tooSmallOs: this.i18n.transform('OS drive must be at least 18 GB'),
        }
      : null
  }

  readonly form = new FormGroup({
    osDrive: new FormControl<DiskInfo | null>(null, [
      Validators.required,
      this.osCapacityValidator,
    ]),
    dataDrive: new FormControl<DiskInfo | null>(null, [Validators.required]),
  })

  readonly dataValidator =
    (osDrive: DiskInfo | null): ValidatorFn =>
    ({ value }: AbstractControl) => {
      if (!value) return null
      const sameAsOs = osDrive && value.logicalname === osDrive.logicalname
      const min = sameAsOs ? this.MIN_BOTH : this.MIN_DATA
      if (value.capacity < min) {
        return sameAsOs
          ? {
              tooSmallBoth: this.i18n.transform(
                'OS + data combined require at least 38 GB',
              ),
            }
          : {
              tooSmallData: this.i18n.transform(
                'Data drive must be at least 20 GB',
              ),
            }
      }
      return null
    }

  drives: DiskInfo[] = []
  loading = true
  shuttingDown = false
  private dialogSub?: Subscription
  preserveData: boolean | null = null

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

    this.form.controls.osDrive.valueChanges.subscribe(drive => {
      if (drive) {
        this.form.controls.osDrive.markAsTouched()
      }
    })

    this.form.controls.dataDrive.valueChanges
      .pipe(distinctUntilChanged())
      .subscribe(drive => {
        this.preserveData = null
        if (drive) {
          this.form.controls.dataDrive.markAsTouched()
          if (toGuid(drive)) {
            this.showPreserveOverwriteDialog()
          }
        }
      })
  }

  async refresh() {
    this.loading = true
    this.form.reset()
    this.preserveData = null
    await this.loadDrives()
  }

  continue() {
    const osDrive = this.form.controls.osDrive.value
    const dataDrive = this.form.controls.dataDrive.value
    if (!osDrive || !dataDrive) return

    const sameDevice = osDrive.logicalname === dataDrive.logicalname
    const dataHasStartOS = !!toGuid(dataDrive)

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
    const drive = this.form.controls.dataDrive.value
    const filesystem =
      drive?.filesystem ||
      drive?.partitions.find(p => p.guid)?.filesystem ||
      null
    const isExt4 = filesystem === 'ext2'

    this.dialogs
      .openComponent<boolean>(PRESERVE_OVERWRITE, {
        data: { isExt4 },
      })
      .subscribe({
        next: preserve => {
          selectionMade = true
          this.preserveData = preserve
          this.cdr.markForCheck()
        },
        complete: () => {
          if (!selectionMade) {
            // Dialog was dismissed without selection - clear the data drive
            this.form.controls.dataDrive.reset()
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
    const osDrive = this.form.controls.osDrive.value!
    const dataDrive = this.form.controls.dataDrive.value!
    const loader = this.loader.open('Installing StartOS').subscribe()

    try {
      const result = await this.api.installOs({
        osDrive: osDrive.logicalname,
        dataDrive: {
          logicalname: dataDrive.logicalname,
          wipe,
        },
      })

      this.stateService.dataDriveGuid = result.guid
      this.stateService.attach = result.attach
      this.stateService.mokEnrolled = result.mokEnrolled

      loader.unsubscribe()

      console.log('Ctrl+Shift+X to shutdown')

      // Show success dialog
      this.dialogSub = this.dialogs
        .openAlert('StartOS has been installed successfully.', {
          label: 'Installation Complete!',
          dismissible: false,
          closable: false,
          data: this.i18n.transform('Continue to Setup'),
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
