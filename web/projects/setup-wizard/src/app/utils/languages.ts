import { i18nKey } from '@start9labs/shared'

export interface Keyboard {
  code: string
  name: string
}

export interface Language {
  code: string
  tuiName: i18nKey
  nativeName: string
}

export const LANGUAGES: Language[] = [
  { code: 'en', tuiName: 'english', nativeName: 'English' },
  { code: 'es', tuiName: 'spanish', nativeName: 'Español' },
  { code: 'de', tuiName: 'german', nativeName: 'Deutsch' },
  { code: 'fr', tuiName: 'french', nativeName: 'Français' },
  { code: 'pl', tuiName: 'polish', nativeName: 'Polski' },
]

export const KEYBOARDS_BY_LANGUAGE: Record<string, Keyboard[]> = {
  en: [
    { code: 'us', name: 'US English' },
    { code: 'gb', name: 'UK English' },
  ],
  es: [
    { code: 'es', name: 'Spanish' },
    { code: 'latam', name: 'Latin American' },
  ],
  de: [{ code: 'de', name: 'German' }],
  fr: [{ code: 'fr', name: 'French' }],
  pl: [{ code: 'pl', name: 'Polish' }],
}

/**
 * Get available keyboards for a language.
 * Returns array of keyboards (may be 1 or more).
 */
export function getKeyboardsForLanguage(languageCode: string): Keyboard[] {
  return (
    KEYBOARDS_BY_LANGUAGE[languageCode] || [{ code: 'us', name: 'US English' }]
  )
}

/**
 * Check if keyboard selection is needed for a language.
 * Returns true if there are multiple keyboard options.
 */
export function needsKeyboardSelection(languageCode: string): boolean {
  const keyboards = getKeyboardsForLanguage(languageCode)
  return keyboards.length > 1
}

/**
 * Get the default keyboard for a language.
 * Returns the first keyboard option.
 */
export function getDefaultKeyboard(languageCode: string): Keyboard {
  return getKeyboardsForLanguage(languageCode)[0]!
}
