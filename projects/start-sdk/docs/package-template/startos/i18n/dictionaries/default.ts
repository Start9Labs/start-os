export const DEFAULT_LANG = 'en_US'

const dict = {
  // main.ts
  'Starting {{name}}!': 0,
} as const

/**
 * Plumbing. DO NOT EDIT.
 */
export type I18nKey = keyof typeof dict
export type LangDict = Record<(typeof dict)[I18nKey], string>
export default dict
