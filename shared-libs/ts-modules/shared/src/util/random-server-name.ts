import { ADJECTIVES, NOUNS } from './server-name-words'

/**
 * Generates a random server name in "Adjective Noun" format.
 * Uses the same word lists as the Rust backend (core/src/assets/).
 */
export function randomServerName(): string {
  const adj = ADJECTIVES[Math.floor(Math.random() * ADJECTIVES.length)]!
  const noun = NOUNS[Math.floor(Math.random() * NOUNS.length)]!

  return `${capitalize(adj)} ${capitalize(noun)}`
}

function capitalize(word: string): string {
  return word.charAt(0).toUpperCase() + word.slice(1)
}
