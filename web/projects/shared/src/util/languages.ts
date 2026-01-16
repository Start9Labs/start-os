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
  { code: 'en', name: 'english', nativeName: 'English' },
  { code: 'es', name: 'spanish', nativeName: 'Español' },
  { code: 'de', name: 'german', nativeName: 'Deutsch' },
  { code: 'fr', name: 'french', nativeName: 'Français' },
  { code: 'pl', name: 'polish', nativeName: 'Polski' },
]

/**
 * Maps i18n language names to ISO language codes
 */
export const LANGUAGE_TO_CODE: Record<Languages, LanguageCode> = {
  english: 'en',
  spanish: 'es',
  german: 'de',
  french: 'fr',
  polish: 'pl',
}

/**
 * Params for setting language via API
 */
export interface SetLanguageParams {
  language: Languages
}
