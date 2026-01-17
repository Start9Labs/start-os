import { Languages } from '../i18n/i18n.service'

/**
 * ISO language codes
 */
export type LanguageCode = 'en' | 'es' | 'de' | 'fr' | 'pl'

/**
 * Language definition with metadata
 */
export interface Language {
  code: LanguageCode
  name: Languages
  nativeName: string
}

/**
 * Available languages with their metadata
 */
export const LANGUAGES: Language[] = [
  { code: 'en', name: 'en_US', nativeName: 'English' },
  { code: 'es', name: 'es_ES', nativeName: 'Español' },
  { code: 'de', name: 'de_DE', nativeName: 'Deutsch' },
  { code: 'fr', name: 'fr_FR', nativeName: 'Français' },
  { code: 'pl', name: 'pl_PL', nativeName: 'Polski' },
]

/**
 * Maps POSIX locale strings to ISO language codes
 */
export const LANGUAGE_TO_CODE: Record<Languages, LanguageCode> = {
  en_US: 'en',
  es_ES: 'es',
  de_DE: 'de',
  fr_FR: 'fr',
  pl_PL: 'pl',
}

/**
 * Params for setting language via API
 */
export interface SetLanguageParams {
  language: Languages
}
