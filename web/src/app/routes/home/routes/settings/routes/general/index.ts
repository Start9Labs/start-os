import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  signal,
} from '@angular/core'
import { NonNullableFormBuilder, ReactiveFormsModule } from '@angular/forms'
import { WA_LOCAL_STORAGE } from '@ng-web-apis/common'
import { TuiValueChanges } from '@taiga-ui/cdk'
import {
  TUI_DARK_MODE,
  TUI_DARK_MODE_KEY,
  TuiAppearance,
  TuiButton,
  TuiDataList,
  TuiIcon,
  TuiLabel,
  TuiNotification,
  TuiNotificationService,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import {
  TuiAccordion,
  TuiChevron,
  TuiDataListWrapper,
  TuiNotificationMiddleService,
  TuiRadio,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import { Footer } from 'src/app/components/footer'
import { MarkdownPipe } from 'src/app/pipes/markdown.pipe'
import { SystemService } from 'src/app/services/system.service'
import {
  ApiService,
  RemoteAccess,
  Theme,
} from 'src/app/services/api/api.service'
import { LANGUAGES, Language, getTranslatedName } from 'src/app/utils/languages'

import { GeneralAside } from './aside'

const THEMES: Record<string, Theme> = {
  System: 'system',
  Dark: 'dark',
  Light: 'light',
}

@Component({
  template: `
    <general-aside *help />
    @if (system.updateAvailable()) {
      <tui-accordion
        tuiAppearance="positive"
        class="update-banner"
        [style.border-radius.rem]="1"
      >
        <button tuiAccordion appearance="">
          <header tuiHeader="h6" [style.left]="0">
            <tui-icon icon="@tui.rocket" />
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
            <button tuiButton appearance="positive" type="button">
              Update now
            </button>
          </div>
        </tui-expand>
      </tui-accordion>
    }
    <form [formGroup]="form" [formLoading]="false" (ngSubmit)="onSubmit()">
      <header tuiHeader="h6"><h2 tuiTitle>Preferences</h2></header>
      <article tuiCardLarge class="g-form">
        <section>
          <div>
            <tui-textfield tuiChevron>
              <label tuiLabel>Theme</label>
              <input
                tuiSelect
                formControlName="theme"
                (tuiValueChanges)="onTheme($event)"
              />
              <tui-data-list-wrapper
                *tuiDropdown
                [items]="['System', 'Dark', 'Light']"
              />
            </tui-textfield>
          </div>
          <div>
            <tui-textfield tuiChevron [stringify]="stringifyLanguage">
              <label tuiLabel>Language</label>
              <input tuiSelect formControlName="language" />
              <tui-data-list *tuiDropdown>
                @for (lang of languages; track lang.posix) {
                  <button tuiOption [value]="lang.posix">
                    <div class="language-option">
                      <span class="native">{{ lang.nativeName }}</span>
                      <span class="translated">
                        {{ getTranslatedName(lang.posix) }}
                      </span>
                    </div>
                  </button>
                }
              </tui-data-list>
            </tui-textfield>
          </div>
        </section>
      </article>
      <header tuiHeader="h6"><h2 tuiTitle>Remote Access</h2></header>
      <article tuiCardLarge class="g-form">
        @if (form.value.remote === 'always') {
          <div tuiNotification appearance="warning">
            This setting is not recommended as your router will be exposed to
            the internet
          </div>
        }
        <div class="remote-options">
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
        </div>
      </article>
      <footer appFooter></footer>
    </form>
  `,
  styles: `
    .update-banner {
      .release-notes {
        color: var(--tui-text-secondary);
        font: var(--tui-font-text-s);

        ::ng-deep {
          h1,
          h2,
          h3,
          h4 {
            margin: 0.75rem 0 0.25rem;
            font: var(--tui-font-text-s);
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

    form > header:not(:first-of-type) {
      margin-top: 1rem;
    }

    .remote-options {
      display: flex;
      flex-wrap: wrap;
      gap: 1rem;

      label {
        text-transform: capitalize;
      }
    }

    .language-option {
      display: flex;
      flex-direction: column;
      line-height: 1.3;

      .native {
        font-weight: 500;
      }

      .translated {
        font: var(--tui-font-text-s);
        color: var(--tui-text-secondary);
      }
    }
  `,
  host: { class: 'g-page' },
  providers: [tuiTextfieldOptionsProvider({ cleaner: signal(false) })],
  imports: [
    ReactiveFormsModule,
    GeneralAside,
    Form,
    Help,
    Footer,
    TuiCardLarge,
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
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class General {
  private readonly api = inject(ApiService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly loading = inject(TuiNotificationMiddleService)
  private readonly mode = inject(TUI_DARK_MODE)
  private readonly localStorage = inject(WA_LOCAL_STORAGE)
  private readonly darkModeKey = inject(TUI_DARK_MODE_KEY)

  protected readonly system = inject(SystemService)
  protected readonly languages = LANGUAGES

  private readonly systemTheme = computed(
    () => !this.localStorage?.getItem(this.darkModeKey),
  )

  protected readonly latestVersion = computed(() => {
    const versions = this.system.newerVersions()
    return versions.length ? versions[versions.length - 1].version : null
  })

  protected readonly form = inject(NonNullableFormBuilder).group({
    theme: this.systemTheme() ? 'System' : this.mode() ? 'Dark' : 'Light',
    language: 'en_US' as Language,
    remote: 'default',
  })

  protected readonly stringifyLanguage = (posix: Language): string => {
    const lang = LANGUAGES.find(l => l.posix === posix)
    return lang?.nativeName || posix
  }

  protected getTranslatedName(posix: Language): string {
    const currentLang = this.form.value.language as Language
    return getTranslatedName(posix, currentLang)
  }

  protected onTheme(theme: string): void {
    if (theme === 'System') {
      this.mode.reset()
    } else {
      this.mode.set(theme === 'Dark')
    }
  }

  async onSubmit(): Promise<void> {
    const loading = this.loading.open('').subscribe()
    try {
      await this.api.setPreferences({
        theme: THEMES[this.form.value.theme!],
        language: this.form.value.language,
        remoteAccess: this.form.value.remote as RemoteAccess,
      })
      this.alerts
        .open('Preferences saved', { appearance: 'positive' })
        .subscribe()
      this.form.markAsPristine()
    } catch (e: any) {
      console.error(e)
      this.alerts
        .open(e.message || 'Failed to save preferences', {
          appearance: 'negative',
        })
        .subscribe()
    } finally {
      loading.unsubscribe()
    }
  }
}
