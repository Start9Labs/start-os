import { AsyncPipe } from '@angular/common'
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
  getAllKeyboardsSorted,
  getKeyboardName,
  i18nKey,
  i18nPipe,
  i18nService,
  Keyboard,
  KeyboardLayout,
  Language,
  LANGUAGES,
  LANGUAGE_TO_CODE,
  LoadingService,
} from '@start9labs/shared'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiAnimated } from '@taiga-ui/cdk'
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
import { TuiCell, tuiCellOptionsProvider } from '@taiga-ui/layout'
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
import { KeyboardSelectComponent } from './keyboard-select.component'

@Component({
  template: `
    <ng-container *title>
      <a routerLink=".." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ 'General Settings' | i18n }}
    </ng-container>
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
          <strong>{{ 'Browser tab title' | i18n }}</strong>
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
            {{ currentLanguage?.nativeName }}
          </span>
        </span>
        <button
          tuiButtonSelect
          tuiButton
          [loading]="i18nService.loading()"
          [ngModel]="currentLanguage"
          (ngModelChange)="onLanguageChange($event)"
        >
          {{ 'Change' | i18n }}
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            size="l"
            [items]="languages"
            [itemContent]="languageContent"
          />
        </button>
        <ng-template #languageContent let-item>
          {{ item.nativeName }}
        </ng-template>
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.monitor" />
        <span tuiTitle>
          <strong>
            {{ 'Kiosk Mode' | i18n }}
            <tui-badge
              size="m"
              [appearance]="server.kiosk ? 'warning' : 'primary-grayscale'"
            >
              {{ server.kiosk ? ('Enabled' | i18n) : ('Disabled' | i18n) }}
            </tui-badge>
          </strong>
          <span tuiSubtitle [class.warning-text]="server.kiosk">
            @if (server.kiosk === null) {
              {{ 'Kiosk Mode is unavailable on this device' | i18n }}
            } @else {
              {{
                server.kiosk
                  ? ('Disable Kiosk Mode unless you need to attach a monitor'
                    | i18n)
                  : ('Enable Kiosk Mode if you need to attach a monitor' | i18n)
              }}
            }
          </span>
          @if (server.kiosk !== null && server.keyboard?.layout; as layout) {
            <span tuiSubtitle class="keyboard-info">
              <tui-icon icon="@tui.keyboard" />
              {{ getKeyboardName(layout) }}
              <button
                tuiIconButton
                appearance="icon"
                iconStart="@tui.pencil"
                size="xs"
                (click)="onChangeKeyboard()"
              ></button>
            </span>
          }
        </span>
        @if (server.kiosk !== null) {
          <button tuiButton appearance="primary" (click)="toggleKiosk()">
            {{ server.kiosk ? ('Disable' | i18n) : ('Enable' | i18n) }}
          </button>
        }
      </div>
      <div tuiCell tuiAppearance="outline-grayscale">
        <tui-icon icon="@tui.rotate-cw" (click)="count = count + 1" />
        <span tuiTitle>
          <strong>{{ 'Restart Tor' | i18n }}</strong>
          <span tuiSubtitle>
            {{ 'Restart the Tor daemon on your server' | i18n }}
          </span>
        </span>
        <button tuiButton appearance="glass" (click)="onTorRestart()">
          {{ 'Restart' | i18n }}
        </button>
      </div>
      @if (count > 4) {
        <div tuiCell tuiAppearance="outline-grayscale" tuiAnimated>
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
        [snek]="score() || 0"
        class="snek"
        alt="Play Snake"
        src="assets/img/icons/snek.png"
      />
    }
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

    [tuiAnimated].tui-enter,
    [tuiAnimated].tui-leave {
      animation-name: tuiFade, tuiScale;
    }

    .keyboard-info {
      display: flex;
      align-items: center;
      gap: 0.25rem;

      tui-icon {
        font-size: 0.875rem;
      }
    }

    .warning-text,
    [tuiSubtitle].warning-text {
      color: var(--tui-status-warning) !important;
    }
  `,
  providers: [tuiCellOptionsProvider({ height: 'spacious' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    AsyncPipe,
    RouterLink,
    i18nPipe,
    TuiTitle,
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
    TuiAnimated,
  ],
})
export default class SystemGeneralComponent {
  private readonly title = inject(Title)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly api = inject(ApiService)
  private readonly isTor = inject(ConfigService).accessType === 'tor'
  private readonly dialog = inject(DialogService)
  private readonly i18n = inject(i18nPipe)
  private readonly injector = inject(INJECTOR)

  wipe = false
  count = 0

  readonly server = toSignal(this.patch.watch$('serverInfo'))
  readonly name = toSignal(this.patch.watch$('ui', 'name'))
  readonly score = toSignal(this.patch.watch$('ui', 'snakeHighScore'))
  readonly os = inject(OSService)
  readonly i18nService = inject(i18nService)
  readonly languages = LANGUAGES

  get currentLanguage(): Language | undefined {
    return LANGUAGES.find(lang => lang.name === this.i18nService.lang)
  }

  onLanguageChange(language: Language) {
    this.i18nService.setLang(language.name)
  }

  // Expose shared utilities for template use
  readonly getKeyboardName = getKeyboardName

  /**
   * Open keyboard selection dialog to change keyboard layout
   */
  onChangeKeyboard() {
    const server = this.server()
    if (!server) return

    const keyboards = getAllKeyboardsSorted(LANGUAGE_TO_CODE[server.language])
    const currentLayout = (server.keyboard?.layout as KeyboardLayout) || null

    this.dialog
      .openComponent<Keyboard | null>(
        new PolymorpheusComponent(KeyboardSelectComponent, this.injector),
        {
          label: 'Select Keyboard Layout',
          size: 's',
          data: { keyboards, currentLayout },
        },
      )
      .pipe(filter((keyboard): keyboard is Keyboard => keyboard !== null))
      .subscribe(keyboard => {
        this.saveKeyboard(keyboard)
      })
  }

  private async saveKeyboard(keyboard: Keyboard) {
    const loader = this.loader.open('Saving').subscribe()

    try {
      await this.api.setKeyboard({
        layout: keyboard.layout,
        keymap: keyboard.keymap,
        model: null,
        variant: null,
        options: [],
      })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
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
        label: 'Browser tab title',
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

  onTorRestart() {
    this.wipe = false
    this.dialog
      .openConfirm({
        label: this.isTor ? 'Warning' : 'Confirm',
        data: {
          content: new PolymorpheusComponent(
            SystemWipeComponent,
            this.injector,
          ),
          yes: 'Restart',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.resetTor(this.wipe))
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

  async toggleKiosk() {
    const server = this.server()
    if (!server) return

    const kiosk = server.kiosk

    // If disabling kiosk, just disable it
    if (kiosk) {
      await this.disableKiosk()
      return
    }

    // Enabling kiosk - check if keyboard is already set
    if (server.keyboard) {
      // Keyboard already set, just enable kiosk
      await this.enableKiosk()
      return
    }

    // No keyboard set - prompt user to select from all keyboards
    const keyboards = getAllKeyboardsSorted(LANGUAGE_TO_CODE[server.language])
    this.promptKeyboardSelection(keyboards)
  }

  private promptKeyboardSelection(keyboards: Keyboard[]) {
    this.dialog
      .openComponent<Keyboard | null>(
        new PolymorpheusComponent(KeyboardSelectComponent, this.injector),
        {
          label: 'Select Keyboard Layout',
          size: 's',
          data: { keyboards, currentLayout: null },
        },
      )
      .pipe(filter((keyboard): keyboard is Keyboard => keyboard !== null))
      .subscribe(keyboard => {
        this.enableKioskWithKeyboard(keyboard)
      })
  }

  private async enableKiosk() {
    const loader = this.loader.open('Enabling').subscribe()

    try {
      await this.api.toggleKiosk(true)
      this.promptRestart()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async enableKioskWithKeyboard(keyboard: Keyboard) {
    const loader = this.loader.open('Enabling').subscribe()

    try {
      await this.api.setKeyboard({
        layout: keyboard.layout,
        keymap: keyboard.keymap,
        model: null,
        variant: null,
        options: [],
      })
      await this.api.toggleKiosk(true)
      this.promptRestart()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async disableKiosk() {
    const loader = this.loader.open('Disabling').subscribe()

    try {
      await this.api.toggleKiosk(false)
      this.promptRestart()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private promptRestart() {
    this.dialog
      .openConfirm({
        label: 'Restart to apply',
        data: {
          content: 'This change will take effect after the next boot',
          yes: 'Restart now',
          no: 'Later',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.restart())
  }

  private async resetTor(wipeState: boolean) {
    const loader = this.loader.open().subscribe()

    try {
      await this.api.resetTor({ wipeState, reason: 'User triggered' })
      this.dialog.openAlert('Tor restart in progress').subscribe()
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
