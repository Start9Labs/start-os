import { Directive, HostListener, inject, Input } from '@angular/core'
import {
  convertAnsi,
  DownloadHTMLService,
  ErrorService,
  FetchLogsReq,
  FetchLogsRes,
  LoadingService,
} from '@start9labs/shared'
import { LogsComponent } from './logs.component'

@Directive({
  standalone: true,
  selector: 'button[logsDownload]',
})
export class LogsDownloadDirective {
  private readonly component = inject(LogsComponent)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly downloadHtml = inject(DownloadHTMLService)

  @Input({ required: true })
  logsDownload!: (params: FetchLogsReq) => Promise<FetchLogsRes>

  @HostListener('click')
  async download() {
    const loader = this.loader.open('Processing 10,000 logs').subscribe()

    try {
      const { entries } = await this.logsDownload({
        before: true,
        limit: 10000,
      })

      this.downloadHtml.download(
        `${this.component.context}-logs.html`,
        convertAnsi(entries),
        STYLES,
      )
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

const STYLES = {
  'background-color': '#222428',
  color: '#e0e0e0',
  'font-family': 'monospace',
}
