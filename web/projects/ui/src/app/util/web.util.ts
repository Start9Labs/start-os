export function strip(html: string) {
  let doc = new DOMParser().parseFromString(html, 'text/html')
  return doc.body.textContent || ''
}
