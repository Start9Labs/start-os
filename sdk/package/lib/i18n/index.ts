/**
 * Internationalization (i18n) utilities for StartOS packages.
 *
 * @example
 * ```typescript
 * // In package's i18n/index.ts:
 * import { setupI18n } from '@start9labs/start-sdk'
 * import defaultDict, { DEFAULT_LANG } from './dictionaries/default'
 * import translations from './dictionaries/translations'
 *
 * export const i18n = setupI18n(defaultDict, translations, DEFAULT_LANG)
 * ```
 */

type ParamValue = string | number | Date

/**
 * Creates a typed i18n function for a package.
 *
 * @param defaultDict - The default language dictionary mapping strings to numeric indices
 * @param translations - Translation dictionaries for each supported locale
 * @param defaultLang - The default language code (e.g., 'en_US')
 * @returns A typed i18n function that accepts dictionary keys and optional parameters
 */
export function setupI18n<
  Dict extends Record<string, number>,
  Translations extends Record<string, Record<number, string>>,
>(defaultDict: Dict, translations: Translations, defaultLang: string) {
  const lang = process.env.LANG?.replace(/\.UTF-8$/, '') || defaultLang

  // Convert locale format from en_US to en-US for Intl APIs
  const intlLocale = lang.replace('_', '-')

  function getTranslation(): Record<number, string> | null {
    if (lang === defaultLang) return null

    const availableLangs = Object.keys(translations) as (keyof Translations)[]

    const match =
      availableLangs.find((l) => l === lang) ??
      availableLangs.find((l) => String(l).startsWith(lang.split('_')[0] + '_'))

    return match ? (translations[match] as Record<number, string>) : null
  }

  const translation = getTranslation()

  function formatValue(value: ParamValue): string {
    if (typeof value === 'number') {
      return new Intl.NumberFormat(intlLocale).format(value)
    }
    if (value instanceof Date) {
      return new Intl.DateTimeFormat(intlLocale).format(value)
    }
    return value
  }

  return function i18n(
    key: keyof Dict,
    params?: Record<string, ParamValue>,
  ): string {
    let result = translation
      ? translation[defaultDict[key as string]]
      : (key as string)

    if (params) {
      for (const [paramName, value] of Object.entries(params)) {
        result = result.replace(`\${${paramName}}`, formatValue(value))
      }
    }

    return result
  }
}
