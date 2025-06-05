import { AsyncPipe, DOCUMENT } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { FormsModule } from '@angular/forms'
import { Title } from '@angular/platform-browser'
import { RouterLink } from '@angular/router'
import {
  DialogService,
  ErrorService,
  i18nKey,
  i18nPipe,
  i18nService,
  languages,
  Languages,
  LoadingService,
} from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiContext, TuiStringHandler } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  tuiFadeIn,
  TuiIcon,
  tuiScaleIn,
  TuiTextfield,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiBadge,
  TuiBadgeNotification,
  TuiButtonLoading,
  TuiButtonSelect,
  TuiDataListWrapper,
} from '@taiga-ui/kit'
import { TuiCell, tuiCellOptionsProvider, TuiHeader } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { OSService } from 'src/app/services/os.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { SnekDirective } from './snek.directive'
import { UPDATE } from './update.component'
import { SystemWipeComponent } from './wipe.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'General Settings' | i18n }}
    </ng-container>
    <header tuiHeader>
      <hgroup tuiTitle>
        <h3>{{ 'General Settings' | i18n }}</h3>
        <p tuiSubtitle>
          {{ 'Manage your overall setup and preferences' | i18n }}
        </p>
      </hgroup>
    </header>
    @if (server(); as server) {
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.zap" />
        <span tuiTitle>
          <strong>{{ 'Software Update' | i18n }}</strong>
          <span tuiSubtitle [style.flex-wrap]="'wrap'">
            {{ server.version }}
            @if (os.showUpdate$ | async) {
              <tui-badge-notification>
                {{ 'Update available' | i18n }}
              </tui-badge-notification>
            }
          </span>
        </span>
        <button
          tuiButton
          appearance="accent"
          iconStart="@tui.refresh-cw"
          [disabled]="os.updatingOrBackingUp$ | async"
          (click)="onUpdate()"
        >
          @if (server.statusInfo.updated) {
            {{ 'Restart to apply' | i18n }}
          } @else {
            @if (os.showUpdate$ | async) {
              {{ 'Update' | i18n }}
            } @else {
              {{ 'Check for updates' | i18n }}
            }
          }
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.app-window" />
        <span tuiTitle>
          <strong>{{ 'Browser Tab Title' | i18n }}</strong>
          <span tuiSubtitle>
            {{ 'Customize the name appearing in your browser tab' | i18n }}
          </span>
        </span>
        <button tuiButton (click)="onTitle()">{{ 'Change' | i18n }}</button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.languages" />
        <span tuiTitle>
          <strong>{{ 'Language' | i18n }}</strong>
          <span tuiSubtitle>
            @if (language; as lang) {
              {{ lang | i18n }}
            } @else {
              {{ i18nService.language }}
            }
          </span>
        </span>
        <button
          tuiButtonSelect
          tuiButton
          [loading]="i18nService.loading()"
          [ngModel]="i18nService.language"
          (ngModelChange)="i18nService.setLanguage($event)"
        >
          {{ 'Change' | i18n }}
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            size="l"
            [items]="languages"
            [itemContent]="translation"
          />
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.award" />
        <span tuiTitle>
          <strong>{{ 'Root Certificate Authority' | i18n }}</strong>
          <span tuiSubtitle>{{ 'Download your Root CA' | i18n }}</span>
        </span>
        <button tuiButton iconStart="@tui.download" (click)="downloadCA()">
          {{ 'Download' | i18n }}
        </button>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.monitor" />
        <span tuiTitle>
          <strong>
            {{ 'Kiosk Mode' | i18n }}
            <tui-badge size="m" appearance="primary-grayscale">
              {{ server.kiosk ? ('Enabled' | i18n) : ('Disabled' | i18n) }}
            </tui-badge>
          </strong>
          <span tuiSubtitle>
            {{
              server.kiosk === true
                ? ('Disable Kiosk Mode unless you need to attach a monitor'
                  | i18n)
                : server.kiosk === false
                  ? ('Enable Kiosk Mode if you need to attach a monitor' | i18n)
                  : ('Kiosk Mode is unavailable on this device' | i18n)
            }}
          </span>
        </span>
        @if (server.kiosk !== null) {
          <button tuiButton appearance="primary" (click)="tryToggleKiosk()">
            {{ server.kiosk ? ('Disable' | i18n) : ('Enable' | i18n) }}
          </button>
        }
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.circle-power" (click)="count = count + 1" />
        <span tuiTitle>
          <strong>{{ 'Reset Tor' | i18n }}</strong>
          <span tuiSubtitle>
            {{ 'Restart the Tor daemon on your server' | i18n }}
          </span>
        </span>
        <button tuiButton appearance="glass" (click)="onReset()">
          {{ 'Reset' | i18n }}
        </button>
      </div>
      @if (count > 4) {
        <div tuiCell tuiAppearance="outline-grayscale" @tuiScaleIn @tuiFadeIn>
          <tui-icon icon="@tui.briefcase-medical" />
          <span tuiTitle>
            <strong>{{ 'Disk Repair' | i18n }}</strong>
            <span tuiSubtitle>{{ 'Attempt automatic repair' | i18n }}</span>
          </span>
          <button tuiButton appearance="glass" (click)="onRepair()">
            {{ 'Repair' | i18n }}
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
  `,
  providers: [tuiCellOptionsProvider({ height: 'spacious' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [tuiScaleIn, tuiFadeIn],
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
    TuiBadge,
    TuiBadgeNotification,
  ],
})
export default class SystemGeneralComponent {
  private readonly title = inject(Title)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly isTor = inject(ConfigService).isTor()
  private readonly document = inject(DOCUMENT)
  private readonly dialog = inject(DialogService)
  private readonly i18n = inject(i18nPipe)
  private readonly injector = inject(INJECTOR)

  wipe = false
  count = 0

  readonly server = toSignal(this.patch.watch$('serverInfo'))
  readonly name = toSignal(this.patch.watch$('ui', 'name'))
  readonly os = inject(OSService)
  readonly i18nService = inject(i18nService)
  readonly languages = languages
  readonly translation: TuiStringHandler<TuiContext<Languages>> = ({
    $implicit,
  }) => this.i18n.transform($implicit)!
  readonly score = toSignal(this.patch.watch$('ui', 'snakeHighScore'), {
    initialValue: 0,
  })

  get language(): Languages | undefined {
    return this.languages.find(lang => lang === this.i18nService.language)
  }

  onUpdate() {
    if (this.server()?.statusInfo.updated) {
      this.restart()
    } else if (this.os.updateAvailable$.value) {
      this.update()
    } else {
      this.check()
    }
  }

  onTitle() {
    const sub = this.dialog
      .openPrompt<string>({
        label: 'Browser Tab Title',
        data: {
          label: 'Device Name',
          message:
            'This value will be displayed as the title of your browser tab.',
          placeholder: 'StartOS' as i18nKey,
          required: false,
          buttonText: 'Save',
          initialValue: this.name(),
        },
      })
      .subscribe(async name => {
        const loader = this.loader.open('Saving').subscribe()
        const title = `${name || 'StartOS'} — ${this.i18n.transform('System')}`

        try {
          await this.api.setDbValue(['name'], name || null)
          this.title.setTitle(title)
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
          sub.unsubscribe()
        }
      })
  }

  onReset() {
    this.wipe = false
    this.dialog
      .openConfirm({
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content: new PolymorpheusComponent(
            SystemWipeComponent,
            this.injector,
          ),
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

  async tryToggleKiosk() {
    if (
      this.server()?.kiosk &&
      ['localhost', '127.0.0.1'].includes(this.document.location.hostname)
    ) {
      return this.dialog
        .openConfirm({
          label: 'Warning',
          data: {
            content:
              'You are currently using a kiosk. Disabling Kiosk Mode will result in the kiosk disconnecting.',
            yes: 'Disable',
            no: 'Cancel',
          },
        })
        .pipe(filter(Boolean))
        .subscribe(async () => this.toggleKiosk())
    }

    this.toggleKiosk()
  }

  async onRepair() {
    this.dialog
      .openConfirm({
        label: 'Warning',
        data: {
          content:
            'This action should only be executed if directed by a Start9 support specialist. We recommend backing up your device before preforming this action. If anything happens to the device during the reboot, such as losing power or unplugging the drive, the filesystem will be in an unrecoverable state. Please proceed with caution.',
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

  private async toggleKiosk() {
    const kiosk = this.server()?.kiosk

    const loader = this.loader
      .open(kiosk ? 'Disabling' : 'Enabling')
      .subscribe()

    try {
      await this.api.toggleKiosk(!kiosk)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open('Resetting Tor').subscribe()

    try {
      await this.api.resetTor({ wipeState, reason: 'User triggered' })
      this.dialog.openAlert('Tor reset in progress').subscribe()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private update() {
    this.dialogs
      .open(UPDATE, {
        data: { currentVersion: this.server()?.version },
      })
      .subscribe()
  }

  private async check(): Promise<void> {
    const loader = this.loader.open('Checking for updates').subscribe()

    try {
      await this.os.loadOS()

      if (this.os.updateAvailable$.value) {
        this.update()
      } else {
        this.dialog
          .openAlert('You are on the latest version of StartOS.', {
            label: 'Up to date!',
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
    const loader = this.loader.open('Beginning restart').subscribe()

    try {
      await this.api.restartServer({})
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
