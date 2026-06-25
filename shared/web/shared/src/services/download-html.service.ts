import { Inject, Injectable, DOCUMENT } from '@angular/core'

@Injectable({
  providedIn: 'root',
})
export class DownloadHTMLService {
  constructor(@Inject(DOCUMENT) private readonly document: Document) {}

  async download(filename: string, html: string, styleObj = {}) {
    const entries = Object.entries(styleObj)
      .map(([k, v]) => `${k}:${v}`)
      .join(';')
    const styleString = entries ? `<style>html{${entries}}></style>` : ''

    html = styleString + html

    const elem = this.document.createElement('a')
    elem.setAttribute(
      'href',
      URL.createObjectURL(
        new Blob([html], { type: 'application/octet-stream' }),
      ),
    )
    elem.setAttribute('download', filename)
    elem.style.display = 'none'

    this.document.body.appendChild(elem)
    elem.click()
    this.document.body.removeChild(elem)
  }
}
