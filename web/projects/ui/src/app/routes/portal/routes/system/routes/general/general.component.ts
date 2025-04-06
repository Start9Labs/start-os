import { AsyncPipe, DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
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
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TUI_CONFIRM,
  TuiButtonLoading,
  TuiButtonSelect,
  TuiDataListWrapper,
} from '@taiga-ui/kit'
import { TuiCell, tuiCellOptionsProvider, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { i18nPipe } from 'src/app/i18n/i18n.pipe'
import { i18nService } from 'src/app/i18n/i18n.service'
import { PROMPT } from 'src/app/routes/portal/modals/prompt.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { EOSService } from 'src/app/services/eos.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { SnekDirective } from './snek.directive'
import { UPDATE } from './update.component'
import { SystemWipeComponent } from './wipe.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'ui.back' | i18n }}
      </a>
      {{ 'system.general.title' | i18n }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>
          {{ 'system.general.title' | i18n }}
        </h3>
        <p tuiSubtitle>
          {{ 'system.general.subtitle' | i18n }}
        </p>
      </hgroup>
    </header>
    @if (server(); as server) {
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.zap" />
        <span tuiTitle>
          <strong>
            {{ 'system.general.update.title' | i18n }}
          </strong>
          <span tuiSubtitle>{{ server.version }}</span>
        </span>
        <button
          tuiButton
          appearance="accent"
          iconStart="@tui.refresh-cw"
          [disabled]="eos.updatingOrBackingUp$ | async"
          (click)="onUpdate()"
        >
          @if (server.statusInfo.updated) {
            {{ 'system.general.update.button.restart' | i18n }}
          } @else {
            @if (eos.showUpdate$ | async) {
              {{ 'ui.update' | i18n }}
            } @else {
              {{ 'system.general.update.button.check' | i18n }}
            }
          }
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.app-window" />
        <span tuiTitle>
          <strong>
            {{ 'system.general.tab' | i18n }}
          </strong>
          <span tuiSubtitle>{{ name() }}</span>
        </span>
        <button tuiButton (click)="onTitle()">
          {{ 'ui.change' | i18n }}
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.languages" />
        <span tuiTitle>
          <strong>
            {{ 'system.general.language' | i18n }}
          </strong>
          <span tuiSubtitle>{{ i18n.language }}</span>
        </span>
        <button
          tuiButtonSelect
          tuiButton
          [loading]="i18n.loading()"
          [ngModel]="i18n.language"
          (ngModelChange)="i18n.setLanguage($event)"
        >
          {{ 'ui.change' | i18n }}
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            size="l"
            [items]="languages"
          />
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.award" />
        <span tuiTitle>
          <strong>
            {{ 'system.general.ca.title' | i18n }}
          </strong>
          <span tuiSubtitle>
            {{ 'system.general.ca.subtitle' | i18n }}
          </span>
        </span>
        <button tuiButton iconStart="@tui.download" (click)="downloadCA()">
          {{ 'system.general.ca.button' | i18n }}
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.circle-power" (click)="count = count + 1" />
        <span tuiTitle>
          <strong>
            {{ 'system.general.tor.title' | i18n }}
          </strong>
          <span tuiSubtitle>
            {{ 'system.general.tor.subtitle' | i18n }}
          </span>
        </span>
        <button tuiButton appearance="glass" (click)="onReset()">
          {{ 'ui.reset' | i18n }}
        </button>
      </div>
      @if (count > 4) {
        <div tuiCell tuiAppearance="outline-grayscale" @tuiScaleIn @tuiFadeIn>
          <tui-icon icon="@tui.briefcase-medical" />
          <span tuiTitle>
            <strong>
              {{ 'system.general.repair.title' | i18n }}
            </strong>
            <span tuiSubtitle>
              {{ 'system.general.repair.subtitle' | i18n }}
            </span>
          </span>
          <button tuiButton appearance="glass" (click)="onRepair()">
            {{ 'system.general.repair.button' | i18n }}
          </button>
        </div>
      }
      <img
        [snek]="score()"
        class="snek"
        alt="Play Snake"
        src="assets/img/icons/snek.png"
      />
    }
    <!-- hidden element for downloading cert -->
    <a id="download-ca" href="/static/local-root-ca.crt"></a>
  `,
  styles: `
    :host {
      max-inline-size: 40rem;
    }

    .snek {
      width: 1rem;
      opacity: 0.2;
      cursor: pointer;

      &:hover {
        opacity: 1;
      }
    }

    strong {
      line-height: 1.25rem;
    }

    [tuiCell] {
      background: var(--tui-background-neutral-1);
    }

    [tuiSubtitle],
    tui-data-list-wrapper ::ng-deep [tuiOption] {
      text-transform: capitalize;
    }
  `,
  providers: [tuiCellOptionsProvider({ height: 'spacious' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiScaleIn, tuiFadeIn],
  standalone: true,
  imports: [
    AsyncPipe,
    RouterLink,
    i18nPipe,
    TuiTitle,
    TuiHeader,
    TuiCell,
    TuiAppearance,
    TuiButton,
    TuiIcon,
    TitleDirective,
    TuiButtonLoading,
    TuiButtonSelect,
    TuiDataListWrapper,
    TuiTextfield,
    FormsModule,
    SnekDirective,
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
  private readonly document = inject(DOCUMENT)

  wipe = false
  count = 0

  readonly server = toSignal(this.patch.watch$('serverInfo'))
  readonly name = toSignal(this.patch.watch$('ui', 'name'))
  readonly eos = inject(EOSService)
  readonly i18n = inject(i18nService)
  readonly languages = ['english', 'spanish']
  readonly score = toSignal(
    this.patch.watch$('ui', 'gaming', 'snake', 'highScore'),
    { initialValue: 0 },
  )

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

  downloadCA() {
    this.document.getElementById('download-ca')?.click()
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
