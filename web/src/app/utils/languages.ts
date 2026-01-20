export const languages = ['en_US', 'es_ES', 'de_DE', 'fr_FR', 'pl_PL'] as const
export type Language = (typeof languages)[number]

/**
 * Language definition with metadata
 */
export interface LanguageInfo {
  posix: Language
  nativeName: string
}

/**
 * Available languages with their metadata
 */
export const LANGUAGES: LanguageInfo[] = [
  { posix: 'en_US', nativeName: 'English' },
  { posix: 'es_ES', nativeName: 'Español' },
  { posix: 'de_DE', nativeName: 'Deutsch' },
  { posix: 'fr_FR', nativeName: 'Français' },
  { posix: 'pl_PL', nativeName: 'Polski' },
]

/**
 * Translations of language names in each language
 */
export const LANGUAGE_TRANSLATIONS: Record<
  Language,
  Record<Language, string>
> = {
  en_US: {
    en_US: 'English',
    es_ES: 'Spanish',
    de_DE: 'German',
    fr_FR: 'French',
    pl_PL: 'Polish',
  },
  es_ES: {
    en_US: 'Inglés',
    es_ES: 'Español',
    de_DE: 'Alemán',
    fr_FR: 'Francés',
    pl_PL: 'Polaco',
  },
  de_DE: {
    en_US: 'Englisch',
    es_ES: 'Spanisch',
    de_DE: 'Deutsch',
    fr_FR: 'Französisch',
    pl_PL: 'Polnisch',
  },
  fr_FR: {
    en_US: 'Anglais',
    es_ES: 'Espagnol',
    de_DE: 'Allemand',
    fr_FR: 'Français',
    pl_PL: 'Polonais',
  },
  pl_PL: {
    en_US: 'Angielski',
    es_ES: 'Hiszpański',
    de_DE: 'Niemiecki',
    fr_FR: 'Francuski',
    pl_PL: 'Polski',
  },
}

/**
 * Get the translated name for a language in the current locale
 */
export function getTranslatedName(
  posix: Language,
  currentLocale: Language,
): string {
  return LANGUAGE_TRANSLATIONS[currentLocale]?.[posix] || posix
}
