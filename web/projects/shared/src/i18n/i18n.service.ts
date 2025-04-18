import { inject, Injectable, signal } from '@angular/core'
import { TuiLanguageName, TuiLanguageSwitcherService } from '@taiga-ui/i18n'
import { I18N, I18N_LOADER, I18N_STORAGE } from './i18n.providers'

@Injectable({
  providedIn: 'root',
})
export class i18nService extends TuiLanguageSwitcherService {
  private readonly i18n = inject(I18N)
  private readonly i18nLoader = inject(I18N_LOADER)
  private readonly store = inject(I18N_STORAGE)

  readonly loading = signal(false)

  override setLanguage(language: TuiLanguageName = 'english'): void {
    if (this.language === language) {
      return
    }

    super.setLanguage(language)
    this.loading.set(true)
    this.store(language).then(() =>
      this.i18nLoader(language).then(value => {
        this.i18n.set(value)
        this.loading.set(false)
      }),
    )
  }
}

export const languages = ['english', 'spanish', 'polish', 'german'] as const
export type Languages = (typeof languages)[number]
