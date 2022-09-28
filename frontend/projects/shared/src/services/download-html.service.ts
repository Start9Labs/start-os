import { DOCUMENT } from '@angular/common'
import { Inject, Injectable } from '@angular/core'

@Injectable()
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
      'data:text/plain;charset=utf-8,' + encodeURIComponent(html),
    )
    elem.setAttribute('download', filename)
    elem.style.display = 'none'

    this.document.body.appendChild(elem)
    elem.click()
    this.document.body.removeChild(elem)
  }
}
