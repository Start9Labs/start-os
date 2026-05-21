import {
  ChangeDetectionStrategy,
  Component,
  computed,
  effect,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_LOCAL_STORAGE } from '@ng-web-apis/common'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiAnimated, TuiValueChanges } from '@taiga-ui/cdk'
import {
  TUI_DARK_MODE,
  TUI_DARK_MODE_KEY,
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiIcon,
  TuiLabel,
  TuiNotification,
  TuiRadio,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import {
  TuiAccordion,
  TuiChevron,
  TUI_CONFIRM,
  TuiDataListWrapper,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiElasticContainer, TuiHeader } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/components/form'
import { MarkdownPipe } from 'src/app/pipes/markdown.pipe'
import { ActionService } from 'src/app/services/action.service'
import {
  ApiService,
  RemoteAccess,
  Theme,
} from 'src/app/services/api/api.service'
import { SystemService } from 'src/app/services/system.service'
import { UPDATE_PROGRESS_DIALOG } from './update-progress-dialog'
import { getTranslatedName, Language, LANGUAGES } from 'src/app/utils/languages'
import { GIT_HASH } from 'src/app/utils/workspace-config'
import {
  formatOffset,
  getPosixTz,
  resolveTimezone,
  TIMEZONES,
} from 'src/app/utils/timezones'

const THEMES: Theme[] = ['system', 'dark', 'light']

@Component({
  template: `
    @if (system.updateAvailable()) {
      <tui-accordion
        tuiAppearance="positive"
        class="update-banner"
        [style.border-radius.rem]="1"
      >
        <button tuiAccordion appearance="">
          <tui-icon icon="@tui.rocket" />
          <header tuiHeader="h6">
            <h2 tuiTitle>v{{ latestVersion() }} released!</h2>
          </header>
        </button>
        <tui-expand>
          <div class="release-notes">
            @for (v of system.newerVersions(); track v.version) {
              <h2>v{{ v.version }}</h2>
              <div [innerHTML]="v.releaseNotes | markdown | dompurify"></div>
            }
          </div>
          <div [style.margin-top.rem]="1">
            <button
              tuiButton
              appearance="positive"
              type="button"
              [disabled]="system.updating()"
              (click)="onUpdate()"
            >
              Update now
            </button>
          </div>
        </tui-expand>
      </tui-accordion>
    }
    <form
      [formGroup]="form"
      [formLoading]="false"
      (reset.prevent)="onCancel()"
      (ngSubmit)="onSubmit()"
    >
      <fieldset>
        <legend>Preferences</legend>
        <section>
          <tui-textfield tuiChevron [stringify]="stringifyTheme">
            <label tuiLabel>Theme</label>
            <input
              tuiSelect
              formControlName="theme"
              (tuiValueChanges)="onTheme($event)"
            />
            <tui-data-list-wrapper *tuiDropdown [items]="themes" />
          </tui-textfield>
          <tui-textfield tuiChevron [stringify]="stringifyLanguage">
            <label tuiLabel>Language</label>
            <input tuiSelect formControlName="language" />
            <tui-data-list *tuiDropdown>
              @for (lang of languages; track lang.posix) {
                <button tuiOption [value]="lang.posix">
                  <span tuiTitle>
                    {{ lang.nativeName }}
                    <span tuiSubtitle>{{ getTranslatedName(lang.posix) }}</span>
                  </span>
                </button>
              }
            </tui-data-list>
          </tui-textfield>
          <tui-textfield tuiChevron [stringify]="stringifyTimezone">
            <label tuiLabel>Timezone</label>
            <input tuiSelect formControlName="timezone" />
            <tui-data-list *tuiDropdown>
              @for (tz of timezones; track tz.iana) {
                <button tuiOption [value]="tz.iana">
                  <span tuiTitle>
                    ({{ formatOffset(tz.offsetMin) }}) {{ tz.label }}
                    <span tuiSubtitle>{{ tz.iana }}</span>
                  </span>
                </button>
              }
            </tui-data-list>
          </tui-textfield>
        </section>
      </fieldset>
      <fieldset>
        <legend>Remote Access</legend>
        @for (value of ['default', 'never', 'always']; track $index) {
          <label tuiLabel>
            <input
              type="radio"
              tuiRadio
              formControlName="remote"
              [value]="value"
            />
            {{ $index ? value : 'When behind NAT (Default)' }}
          </label>
        }
      </fieldset>
      <fieldset>
        <legend>Security</legend>
        <section class="ca-section">
          <p>
            Download your Root CA to trust HTTPS connections from additional
            devices.
          </p>
          <a
            tuiButton
            size="s"
            iconEnd="@tui.download"
            href="/static/root-ca.crt"
            download="startwrt-ca.crt"
          >
            Download Root CA
          </a>
        </section>
      </fieldset>
      <fieldset>
        <legend>About</legend>
        <section class="about-section">
          <dl>
            <dt>Version</dt>
            <dd>{{ system.info()?.version || '—' }}</dd>
            <dt>Build</dt>
            <dd>
              <code [title]="gitHash || ''">{{ shortGitHash() }}</code>
            </dd>
          </dl>
        </section>
      </fieldset>
      <tui-elastic-container>
        @if (form.value.remote === 'always') {
          <div tuiAnimated tuiNotification appearance="warning">
            This setting is not recommended as your router will be exposed to
            the internet
          </div>
        }
      </tui-elastic-container>
      <footer appFooter></footer>
    </form>
  `,
  styles: `
    :host {
      max-width: 50rem;
    }

    .update-banner {
      .release-notes {
        color: var(--tui-text-secondary);
        font: var(--tui-typography-body-s);

        ::ng-deep {
          h1,
          h2,
          h3,
          h4 {
            margin: 0.75rem 0 0.25rem;
            font: var(--tui-typography-body-s);
            font-weight: bold;
            color: var(--tui-text-primary);

            &:first-child {
              margin-top: 0;
            }
          }

          ul {
            margin: 0;
            padding-left: 1.25rem;
          }

          li {
            margin-bottom: 0.25rem;
          }

          p {
            margin: 0.5rem 0;

            &:first-child {
              margin-top: 0;
            }
          }
        }
      }
    }

    :host {
      fieldset {
        display: flex;
      }

      label {
        text-transform: capitalize;
      }

      .ca-section {
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        gap: 0.5rem;

        p {
          margin: 0;
          color: var(--tui-text-secondary);
          font: var(--tui-typography-body-s);
        }
      }

      .about-section {
        dl {
          display: grid;
          grid-template-columns: max-content 1fr;
          gap: 0.25rem 1rem;
          margin: 0;
        }

        dt {
          color: var(--tui-text-secondary);
          font: var(--tui-typography-body-s);
        }

        dd {
          margin: 0;
          font: var(--tui-typography-body-s);
        }

        code {
          font-family: var(--tui-font-text-mono, monospace);
        }
      }
    }
  `,
  host: { class: 'g-page' },
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    ReactiveFormsModule,
    Form,
    Footer,
    TuiHeader,
    TuiTitle,
    TuiButton,
    TuiDataList,
    TuiIcon,
    TuiLabel,
    TuiNotification,
    TuiAppearance,
    TuiTextfield,
    TuiChevron,
    TuiRadio,
    TuiSelect,
    TuiDataListWrapper,
    TuiValueChanges,
    TuiAccordion,
    MarkdownPipe,
    NgDompurifyPipe,
    TuiElasticContainer,
    TuiAnimated,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class General {
  private readonly api = inject(ApiService)
  private readonly actions = inject(ActionService)
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly mode = inject(TUI_DARK_MODE)

  protected readonly system = inject(SystemService)
  protected readonly gitHash = inject(GIT_HASH)
  protected readonly languages = LANGUAGES

  protected readonly shortGitHash = computed(() =>
    this.gitHash ? this.gitHash.slice(0, 12) : 'unknown',
  )

  protected readonly latestVersion = computed(() => {
    const versions = this.system.newerVersions()
    return versions.length ? versions[versions.length - 1].version : null
  })

  protected readonly form = inject(NonNullableFormBuilder).group({
    theme: (!inject(WA_LOCAL_STORAGE)?.getItem(inject(TUI_DARK_MODE_KEY))
      ? 'system'
      : this.mode()
        ? 'dark'
        : 'light') as Theme,
    language: 'en_US' as Language,
    remote: 'default',
    timezone: Intl.DateTimeFormat().resolvedOptions().timeZone,
  })

  constructor() {
    effect(() => {
      const info = this.system.info()
      if (info && this.form.pristine) {
        this.form.reset({
          theme: info.theme,
          language: info.language as Language,
          remote: info.remoteAccess ?? 'default',
          timezone: resolveTimezone(
            info.timezone || Intl.DateTimeFormat().resolvedOptions().timeZone,
          ),
        })
      }
    })
  }

  protected readonly themes = THEMES
  protected readonly timezones = TIMEZONES

  protected readonly stringifyTheme = (t: Theme): string =>
    t ? t[0].toUpperCase() + t.slice(1) : ''

  protected readonly stringifyLanguage = (posix: Language): string =>
    LANGUAGES.find(l => l.posix === posix)?.nativeName || posix

  protected readonly formatOffset = formatOffset

  protected readonly stringifyTimezone = (iana: string): string => {
    const tz = TIMEZONES.find(t => t.iana === iana)
    return tz ? `(${formatOffset(tz.offsetMin)}) ${tz.label}` : iana
  }

  protected getTranslatedName(posix: Language): string {
    return getTranslatedName(posix, this.form.getRawValue().language)
  }

  protected onTheme(theme: Theme): void {
    if (theme === 'system') {
      this.mode.reset()
    } else {
      this.mode.set(theme === 'dark')
    }
  }

  protected onCancel(): void {
    const info = this.system.info()
    if (info) {
      this.form.reset({
        theme: info.theme,
        language: info.language as Language,
        remote: info.remoteAccess ?? 'default',
        timezone:
          info.timezone || Intl.DateTimeFormat().resolvedOptions().timeZone,
      })
      this.onTheme(info.theme)
    }
  }

  async onSubmit(): Promise<void> {
    await this.actions.run(
      async () => {
        await this.api.setPreferences({
          theme: this.form.value.theme!,
          language: this.form.value.language,
          remoteAccess: this.form.value.remote as RemoteAccess,
        })

        const timezone = this.form.value.timezone!
        const posixTz = getPosixTz(timezone)
        if (posixTz) {
          await this.api.setTimezone({ timezone, posixTz })
        }

        await this.system.refresh()
      },
      {
        fail: 'Failed to save preferences',
        success: 'Preferences saved',
      },
    )
  }

  protected onUpdate(): void {
    const latest = this.system.newerVersions().at(-1)
    if (!latest) return

    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Start System Update?',
        data: {
          content: `Updating to v${latest.version} will restart your router. This may take several minutes and your network will be temporarily unavailable.`,
          yes: 'Update Now',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.doUpdate(latest.version))
  }

  private doUpdate(version: string): void {
    this.dialogs
      .open(UPDATE_PROGRESS_DIALOG, {
        label: 'System Update',
        data: version,
        closable: false,
        dismissible: false,
      })
      .subscribe()
  }
}
