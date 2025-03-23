import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import {
  TuiAlertService,
  TuiAppearance,
  TuiButton,
  tuiFadeIn,
  TuiIcon,
  tuiScaleIn,
  TuiTitle,
} from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiCell, tuiCellOptionsProvider, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { PROMPT } from 'src/app/routes/portal/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { EOSService } from 'src/app/services/eos.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { SystemSyncComponent } from './sync.component'
import { UPDATE } from './update.component'
import { SystemWipeComponent } from './wipe.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      General Settings
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>General</h3>
        <p tuiSubtitle>Manage your overall setup and preferences</p>
      </hgroup>
    </header>
    @if (server(); as server) {
      @if (!server.ntpSynced) {
        <system-sync />
      }
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.zap" />
        <span tuiTitle>
          <strong>Software Update</strong>
          <span tuiSubtitle>{{ server.version }}</span>
        </span>
        <button
          tuiButton
          appearance="accent"
          [disabled]="eos.updatingOrBackingUp$ | async"
          (click)="onUpdate()"
        >
          @if (server.statusInfo.updated) {
            Restart to apply
          } @else {
            @if (eos.showUpdate$ | async) {
              Update
            } @else {
              Check for updates
            }
          }
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.app-window" />
        <span tuiTitle>
          <strong>Browser Tab Title</strong>
          <span tuiSubtitle>{{ name() }}</span>
        </span>
        <button tuiButton (click)="onTitle()">Change</button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.languages" />
        <span tuiTitle>
          <strong>Language</strong>
          <span tuiSubtitle>English</span>
        </span>
        <button tuiButton>Change</button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.circle-power" (click)="count = count + 1" />
        <span tuiTitle>
          <strong>Reset Tor</strong>
          <span tuiSubtitle>Restart the Tor daemon on your server</span>
        </span>
        <button tuiButton appearance="glass" (click)="onReset()">Reset</button>
      </div>
      @if (count > 4) {
        <div tuiCell tuiAppearance="outline-grayscale" @tuiScaleIn @tuiFadeIn>
          <tui-icon icon="@tui.briefcase-medical" />
          <span tuiTitle>
            <strong>Disk Repair</strong>
            <span tuiSubtitle>Attempt automatic repair</span>
          </span>
          <button tuiButton appearance="glass" (click)="onRepair()">
            Repair
          </button>
        </div>
      }
    }
  `,
  styles: `
    :host {
      max-inline-size: 40rem;
    }

    strong {
      line-height: 1.25rem;
    }

    [tuiCell] {
      background: var(--tui-background-neutral-1);
    }
  `,
  providers: [tuiCellOptionsProvider({ height: 'spacious' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiScaleIn, tuiFadeIn],
  standalone: true,
  imports: [
    AsyncPipe,
    RouterLink,
    TuiTitle,
    TuiHeader,
    TuiCell,
    TuiAppearance,
    TuiButton,
    TitleDirective,
    SystemSyncComponent,
    TuiIcon,
  ],
})
export default class SystemGeneralComponent {
  private readonly alerts = inject(TuiAlertService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly isTor = inject(ConfigService).isTor()
  private readonly reset = new PolymorpheusComponent(
    SystemWipeComponent,
    inject(INJECTOR),
  )

  wipe = false
  count = 0

  readonly server = toSignal(this.patch.watch$('serverInfo'))
  readonly name = toSignal(this.patch.watch$('ui', 'name'))
  readonly eos = inject(EOSService)

  onUpdate() {
    if (this.server()?.statusInfo.updated) {
      this.restart()
    } else if (this.eos.updateAvailable$.value) {
      this.update()
    } else {
      this.check()
    }
  }

  onTitle() {
    this.dialogs
      .open<string>(PROMPT, {
        label: 'Browser Tab Title',
        data: {
          message: `This value will be displayed as the title of your browser tab.`,
          label: 'Device Name',
          placeholder: 'StartOS',
          required: false,
          buttonText: 'Save',
          initialValue: this.name(),
        },
      })
      .subscribe(async name => {
        const loader = this.loader.open('Saving...').subscribe()

        try {
          await this.api.setDbValue(['name'], name || null)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  onReset() {
    this.wipe = false
    this.dialogs
      .open(TUI_CONFIRM, {
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content: this.reset,
          yes: 'Reset',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.resetTor(this.wipe))
  }

  async onRepair() {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Warning',
        data: {
          content: `<p>This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action.</p><p>If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem <i>will</i> be in an unrecoverable state. Please proceed with caution.</p>`,
          yes: 'Repair',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        try {
          await this.api.repairDisk({})
          this.restart()
        } catch (e: any) {
          this.errorService.handleError(e)
        }
      })
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open('Resetting Tor...').subscribe()

    try {
      await this.api.resetTor({ wipeState, reason: 'User triggered' })

      this.alerts.open('Tor reset in progress').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private update() {
    this.dialogs.open(UPDATE).subscribe()
  }

  private async check(): Promise<void> {
    const loader = this.loader.open('Checking for updates').subscribe()

    try {
      await this.eos.loadEos()

      if (this.eos.updateAvailable$.value) {
        this.update()
      } else {
        this.dialogs
          .open('You are on the latest version of StartOS.', {
            label: 'Up to date!',
            size: 's',
          })
          .subscribe()
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async restart() {
    const loader = this.loader.open(`Beginning restart...`).subscribe()

    try {
      await this.api.restartServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
