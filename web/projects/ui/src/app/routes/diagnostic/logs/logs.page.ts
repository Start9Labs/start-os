import { Component, ElementRef, inject, OnInit, ViewChild } from '@angular/core'
import { INTERSECTION_ROOT } from '@ng-web-apis/intersection-observer'
import { convertAnsi, ErrorService } from '@start9labs/shared'
import { TuiScrollbar } from '@taiga-ui/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'logs',
  templateUrl: './logs.page.html',
  styles: `
    :host {
      max-height: 100vh;
      display: flex;
      flex-direction: column;
      justify-content: flex-start;
      padding: 1rem;
      gap: 1rem;
      background: var(--tui-background-base);
    }
  `,
  providers: [
    {
      provide: INTERSECTION_ROOT,
      useExisting: ElementRef,
    },
  ],
})
export class LogsPage implements OnInit {
  @ViewChild(TuiScrollbar, { read: ElementRef })
  private readonly scrollbar?: ElementRef<HTMLElement>
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  startCursor?: string
  loading = false
  logs: string[] = []
  scrollTop = 0

  ngOnInit() {
    this.getLogs()
  }

  onTop(top: boolean) {
    if (top) this.getLogs()
  }

  restoreScroll() {
    if (this.loading || !this.scrollbar) return

    const scrollbar = this.scrollbar.nativeElement
    const offset = scrollbar.querySelector('pre')?.clientHeight || 0

    scrollbar.scrollTop = this.scrollTop + offset
  }

  private async getLogs() {
    if (this.loading) return

    this.loading = true

    try {
      const response = await this.api.diagnosticGetLogs({
        cursor: this.startCursor,
        before: !!this.startCursor,
        limit: 200,
      })

      if (!response.entries.length) return

      this.startCursor = response.startCursor
      this.logs = [convertAnsi(response.entries), ...this.logs]
      this.scrollTop = this.scrollbar?.nativeElement.scrollTop || 0
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }
}
