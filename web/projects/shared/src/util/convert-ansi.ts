import { Log } from '../types/api'
import { toLocalIsoString } from './to-local-iso-string'

const Convert = require('ansi-to-html')
const CONVERT = new Convert({
  bg: 'transparent',
  colors: { 4: 'Cyan' },
  escapeXML: true,
})

export function convertAnsi(entries: readonly Log[]): string {
  return entries
    .map(
      ({ timestamp, message }) =>
        `<b style="color: #FFF">${toLocalIsoString(
          new Date(timestamp),
        )}</b>&nbsp;&nbsp;${CONVERT.toHtml(message)}`,
    )
    .join('<br />')
}
