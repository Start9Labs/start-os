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
  TuiRadio,
  TuiTextfield,
  tuiTextfieldOptionsProvider,
  TuiTitle,
} from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import {
  TuiAccordion,
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { Footer } from 'src/app/components/footer'
import { Form } from 'src/app/directives/form'
import { Help } from 'src/app/directives/help'
import { MarkdownPipe } from 'src/app/pipes/markdown.pipe'
import { ActionService } from 'src/app/services/action.service'
import {
  ApiService,
  RemoteAccess,
  Theme,
} from 'src/app/services/api/api.service'
import { SystemService } from 'src/app/services/system.service'
import { getTranslatedName, Language, LANGUAGES } from 'src/app/utils/languages'

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
      <fieldset>
        <legend>Preferences</legend>
        <section>
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
      @if (form.value.remote === 'always') {
        <div tuiNotification appearance="warning">
          This setting is not recommended as your router will be exposed to the
          internet
        </div>
      }
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

    :host {
      fieldset {
        display: flex;
      }

      label {
        text-transform: capitalize;
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
  private readonly actions = inject(ActionService)
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

  protected readonly stringifyLanguage = (posix: Language): string =>
    LANGUAGES.find(l => l.posix === posix)?.nativeName || posix

  protected getTranslatedName(posix: Language): string {
    return getTranslatedName(posix, this.form.getRawValue().language)
  }

  protected onTheme(theme: string): void {
    if (theme === 'System') {
      this.mode.reset()
    } else {
      this.mode.set(theme === 'Dark')
    }
  }

  async onSubmit(): Promise<void> {
    await this.actions.run(
      () =>
        this.api.setPreferences({
          theme: THEMES[this.form.value.theme!],
          language: this.form.value.language,
          remoteAccess: this.form.value.remote as RemoteAccess,
        }),
      {
        fail: 'Failed to save preferences',
        success: 'Preferences saved',
      },
    )
  }
}
