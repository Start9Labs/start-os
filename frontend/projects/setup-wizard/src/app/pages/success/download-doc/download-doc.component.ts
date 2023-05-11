import { Component, Input } from '@angular/core'

@Component({
  selector: 'download-doc',
  templateUrl: 'download-doc.component.html',
})
export class DownloadDocComponent {
  @Input() lanAddress!: string

  get crtName(): string {
    const hostname = new URL(this.lanAddress).hostname
    return `${hostname}.crt`
  }
}
