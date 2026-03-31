import { LanguageCode } from './languages'

/**
 * Keyboard layout codes (X11/Wayland)
 */
export type KeyboardLayout = 'us' | 'gb' | 'es' | 'latam' | 'de' | 'fr' | 'pl'

/**
 * Keyboard keymap codes (console/TTY)
 */
export type KeyboardKeymap = 'us' | 'uk' | 'es' | 'la' | 'de' | 'fr' | 'pl'

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
 * Keyboard definition with layout and keymap
 */
export interface Keyboard {
  layout: KeyboardLayout
  keymap: KeyboardKeymap
  name: KeyboardName
}

/**
 * Full keyboard configuration for backend API
 */
export interface FullKeyboard {
  layout: KeyboardLayout
  keymap: KeyboardKeymap
  model: string | null
  variant: string | null
  options: string[]
}

/**
 * Keyboard layouts grouped by language code
 */
export const KEYBOARDS_BY_LANGUAGE: Record<LanguageCode, Keyboard[]> = {
  en: [
    { layout: 'us', keymap: 'us', name: 'US English' },
    { layout: 'gb', keymap: 'uk', name: 'UK English' },
  ],
  es: [
    { layout: 'es', keymap: 'es', name: 'Spanish' },
    { layout: 'latam', keymap: 'la', name: 'Latin American' },
  ],
  de: [{ layout: 'de', keymap: 'de', name: 'German' }],
  fr: [{ layout: 'fr', keymap: 'fr', name: 'French' }],
  pl: [{ layout: 'pl', keymap: 'pl', name: 'Polish' }],
}

/**
 * All available keyboard layouts
 */
export const ALL_KEYBOARDS: Keyboard[] = [
  { layout: 'us', keymap: 'us', name: 'US English' },
  { layout: 'gb', keymap: 'uk', name: 'UK English' },
  { layout: 'es', keymap: 'es', name: 'Spanish' },
  { layout: 'latam', keymap: 'la', name: 'Latin American' },
  { layout: 'de', keymap: 'de', name: 'German' },
  { layout: 'fr', keymap: 'fr', name: 'French' },
  { layout: 'pl', keymap: 'pl', name: 'Polish' },
]

/**
 * Get all keyboards sorted with language-specific keyboards first,
 * then remaining keyboards alphabetically by name.
 */
export function getAllKeyboardsSorted(languageCode: LanguageCode): Keyboard[] {
  const languageKeyboards = KEYBOARDS_BY_LANGUAGE[languageCode]
  const languageLayouts = new Set(languageKeyboards.map(kb => kb.layout))
  const otherKeyboards = ALL_KEYBOARDS.filter(
    kb => !languageLayouts.has(kb.layout),
  ).sort((a, b) => a.name.localeCompare(b.name))
  return [...languageKeyboards, ...otherKeyboards]
}

/**
 * Get the display name for a keyboard layout.
 */
export function getKeyboardName(
  layout: KeyboardLayout | string,
): KeyboardName | string {
  const keyboard = ALL_KEYBOARDS.find(kb => kb.layout === layout)
  if (keyboard) return keyboard.name
  return layout // fallback to the layout itself if not found
}
