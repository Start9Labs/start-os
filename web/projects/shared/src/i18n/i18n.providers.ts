import { forwardRef, signal } from '@angular/core'
import { tuiCreateToken, tuiProvide } from '@taiga-ui/cdk'
import {
  TuiLanguageName,
  tuiLanguageSwitcher,
  TuiLanguageSwitcherService,
} from '@taiga-ui/i18n'
import { ENGLISH } from './dictionaries/english'
import { i18nService } from './i18n.service'

export type i18nKey = keyof typeof ENGLISH
export type i18n = Record<(typeof ENGLISH)[i18nKey], string>

export const I18N = tuiCreateToken(signal<i18n | null>(null))
export const I18N_LOADER =
  tuiCreateToken<(lang: TuiLanguageName) => Promise<i18n>>()
export const I18N_STORAGE = tuiCreateToken<
  (lang: TuiLanguageName) => Promise<void>
>(() => Promise.resolve())

export const I18N_PROVIDERS = [
  tuiLanguageSwitcher(async (language: TuiLanguageName): Promise<unknown> => {
    switch (language) {
      case 'spanish':
        return import('@taiga-ui/i18n/languages/spanish')
      case 'polish':
        return import('@taiga-ui/i18n/languages/polish')
      case 'german':
        return import('@taiga-ui/i18n/languages/german')
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
        case 'polish':
          return import('./dictionaries/polish').then(v => v.default)
        case 'german':
          return import('./dictionaries/german').then(v => v.default)
        default:
          return null
      }
    },
  },
  tuiProvide(
    TuiLanguageSwitcherService,
    forwardRef(() => i18nService),
  ),
]
