import { Component, computed, inject, signal } from '@angular/core'
import { Router } from '@angular/router'
import { FormsModule } from '@angular/forms'
import { i18nPipe, i18nService, Language, LANGUAGES } from '@start9labs/shared'
import { TUI_IS_MOBILE } from '@taiga-ui/cdk'
import { TuiButton, TuiTextfield, TuiTitle } from '@taiga-ui/core'
import {
  TuiButtonLoading,
  TuiChevron,
  TuiDataListWrapper,
  TuiSelect,
} from '@taiga-ui/kit'
import { TuiCardLarge, TuiHeader } from '@taiga-ui/layout'
import { ApiService } from '../services/api.service'
import { StateService } from '../services/state.service'

@Component({
  template: `
    <section tuiCardLarge="compact">
      <header tuiHeader>
        <h2 tuiTitle>
          <span class="inline-title">
            <img src="assets/img/icon.png" alt="Start9" />
            {{ 'Welcome to' | i18n }} StartOS
          </span>
          <span tuiSubtitle>{{ 'Select your language' | i18n }}</span>
        </h2>
      </header>
      <tui-textfield
        tuiChevron
        [stringify]="stringify"
        [tuiTextfieldCleaner]="false"
      >
        <label tuiLabel>{{ 'Language' | i18n }}</label>
        @if (mobile) {
          <select
            tuiSelect
            [(ngModel)]="selected"
            [items]="languages"
            (ngModelChange)="onLanguageChange($event)"
          ></select>
        } @else {
          <input
            tuiSelect
            [(ngModel)]="selected"
            (ngModelChange)="onLanguageChange($event)"
          />
        }
        @if (!mobile) {
          <tui-data-list-wrapper
            *tuiTextfieldDropdown
            new
            [items]="languages"
            [itemContent]="itemContent"
          />
        }
      </tui-textfield>

      <ng-template #itemContent let-item>
        @let lang = asLanguage(item);
        <div class="language-item">
          <span>{{ lang.nativeName }}</span>
          <small>{{ lang.name | i18n }}</small>
        </div>
      </ng-template>

      <footer>
        <button
          tuiButton
          [disabled]="!selected"
          [loading]="loading()"
          (click)="continue()"
        >
          {{ 'Continue' | i18n }}
        </button>
      </footer>
    </section>
  `,
  styles: `
    .language-item {
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
    TuiButtonLoading,
    TuiTextfield,
    TuiChevron,
    TuiSelect,
    TuiDataListWrapper,
    TuiHeader,
    TuiTitle,
    i18nPipe,
  ],
})
export default class LanguagePage {
  private readonly router = inject(Router)
  private readonly api = inject(ApiService)
  private readonly stateService = inject(StateService)
  private readonly i18nService = inject(i18nService)

  protected readonly mobile = inject(TUI_IS_MOBILE)
  readonly languages = LANGUAGES

  selected =
    LANGUAGES.find(l => l.code === this.stateService.language) || LANGUAGES[0]

  private readonly saving = signal(false)

  // Show loading when either language is loading or saving is in progress
  readonly loading = computed(() => this.i18nService.loading() || this.saving())

  readonly stringify = (lang: Language) => lang.nativeName
  readonly asLanguage = (item: unknown): Language => item as Language

  constructor() {
    if (this.selected) {
      this.i18nService.setLang(this.selected.name)
    }
  }

  onLanguageChange(language: Language) {
    if (language) {
      this.i18nService.setLang(language.name)
    }
  }

  async continue() {
    if (this.selected) {
      this.stateService.language = this.selected.code

      // Save language to backend
      this.saving.set(true)

      try {
        await this.api.setLanguage({ language: this.selected.name })
        // Always go to keyboard selection
        await this.router.navigate(['/keyboard'])
      } finally {
        this.saving.set(false)
      }
    }
  }
}
