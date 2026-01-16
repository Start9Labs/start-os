import { LanguageCode } from './languages'

/**
 * Keyboard layout codes
 */
export type KeyboardCode = 'us' | 'gb' | 'es' | 'latam' | 'de' | 'fr' | 'pl'

/**
 * Keyboard layout display names
 */
export type KeyboardName =
  | 'US English'
  | 'UK English'
  | 'Spanish'
  | 'Latin American'
  | 'German'
  | 'French'
  | 'Polish'

/**
 * Keyboard layout definition
 */
export interface Keyboard {
  code: KeyboardCode
  name: KeyboardName
}

/**
 * Full keyboard configuration for backend API
 */
export interface FullKeyboard {
  layout: string
  model: string | null
  variant: string | null
  options: string[]
}

/**
 * Keyboard layouts grouped by language code
 */
export const KEYBOARDS_BY_LANGUAGE: Record<LanguageCode, Keyboard[]> = {
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
 * All available keyboard layouts
 */
export const ALL_KEYBOARDS: Keyboard[] = [
  { code: 'us', name: 'US English' },
  { code: 'gb', name: 'UK English' },
  { code: 'es', name: 'Spanish' },
  { code: 'latam', name: 'Latin American' },
  { code: 'de', name: 'German' },
  { code: 'fr', name: 'French' },
  { code: 'pl', name: 'Polish' },
]

/**
 * Get all keyboards sorted with language-specific keyboards first,
 * then remaining keyboards alphabetically by name.
 */
export function getAllKeyboardsSorted(languageCode: LanguageCode): Keyboard[] {
  const languageKeyboards = KEYBOARDS_BY_LANGUAGE[languageCode]
  const languageKeyboardCodes = new Set(languageKeyboards.map(kb => kb.code))
  const otherKeyboards = ALL_KEYBOARDS.filter(
    kb => !languageKeyboardCodes.has(kb.code),
  ).sort((a, b) => a.name.localeCompare(b.name))
  return [...languageKeyboards, ...otherKeyboards]
}

/**
 * Get the display name for a keyboard code.
 */
export function getKeyboardName(
  code: KeyboardCode | string,
): KeyboardName | string {
  const keyboard = ALL_KEYBOARDS.find(kb => kb.code === code)
  if (keyboard) return keyboard.name
  return code // fallback to the code itself if not found
}
