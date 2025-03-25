import { signal } from '@angular/core'
import { tuiCreateToken, tuiProvide } from '@taiga-ui/cdk'
import {
  TuiLanguageName,
  tuiLanguageSwitcher,
  TuiLanguageSwitcherService,
} from '@taiga-ui/i18n'
import ENGLISH from './dictionaries/english'
import { i18nService } from './i18n.service'

export type i18n = typeof ENGLISH

export const I18N = tuiCreateToken(signal(ENGLISH))
export const I18N_LOADER =
  tuiCreateToken<(lang: TuiLanguageName) => Promise<i18n>>()

export const I18N_PROVIDERS = [
  tuiLanguageSwitcher(async (language: TuiLanguageName): Promise<unknown> => {
    switch (language) {
      case 'spanish':
        return import('@taiga-ui/i18n/languages/spanish')
      default:
        return import('@taiga-ui/i18n/languages/english')
    }
  }),
  {
    provide: I18N_LOADER,
    useValue: async (language: TuiLanguageName): Promise<unknown> => {
      switch (language) {
        case 'spanish':
          return import('./dictionaries/spanish').then(v => v.default)
        default:
          return import('./dictionaries/english').then(v => v.default)
      }
    },
  },
  tuiProvide(TuiLanguageSwitcherService, i18nService),
]
