import { T } from '@start9labs/start-sdk'
import { toLocalIsoString } from './to-local-iso-string'
import Convert from 'ansi-to-html'

const CONVERT = new Convert({
  bg: 'transparent',
  colors: { 4: 'Cyan' },
  escapeXML: true,
})

export function convertAnsi(entries: readonly T.LogEntry[]): string {
  return entries
    .map(
      ({ timestamp, message }) =>
        `<b style="color: #FFF">${toLocalIsoString(
          new Date(timestamp),
        )}</b>&nbsp;&nbsp;${CONVERT.toHtml(message)}`,
    )
    .join('<br />')
}
