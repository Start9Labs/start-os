import { inject, Injectable, signal } from '@angular/core'
import { TuiLanguageName, TuiLanguageSwitcherService } from '@taiga-ui/i18n'
import { I18N, I18N_LOADER, I18N_STORAGE } from './i18n.providers'

export const languages = ['en_US', 'es_ES', 'de_DE', 'fr_FR', 'pl_PL'] as const
export type Languages = (typeof languages)[number]

/**
 * Maps POSIX locale strings to TUI language names
 */
export const LANGUAGE_TO_TUI: Record<Languages, TuiLanguageName> = {
  en_US: 'english',
  es_ES: 'spanish',
  de_DE: 'german',
  fr_FR: 'french',
  pl_PL: 'polish',
}

@Injectable({
  providedIn: 'root',
})
export class i18nService extends TuiLanguageSwitcherService {
  private readonly i18n = inject(I18N)
  private readonly i18nLoader = inject(I18N_LOADER)
  private readonly store = inject(I18N_STORAGE)

  readonly loading = signal(false)

  /**
   * Current language as POSIX locale string
   */
  get lang(): Languages {
    return (
      (Object.entries(LANGUAGE_TO_TUI).find(
        ([, tui]) => tui === this.language,
      )?.[0] as Languages) || 'en_US'
    )
  }

  setLang(language: Languages = 'en_US'): void {
    const tuiLang = LANGUAGE_TO_TUI[language]
    const current = this.language

    super.setLanguage(tuiLang)
    this.loading.set(true)

    if (current === tuiLang) {
      this.i18nLoader(tuiLang).then(value => {
        this.i18n.set(value)
        this.loading.set(false)
      })
    } else {
      this.store(language).then(() =>
        this.i18nLoader(tuiLang).then(value => {
          this.i18n.set(value)
          this.loading.set(false)
        }),
      )
    }
  }
}
