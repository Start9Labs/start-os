import { Component, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { from, map, switchMap, takeUntil, tap } from 'rxjs'
import { ApiService } from 'src/app/services/api/api.service'
import { DestroyService, Log, toLocalIsoString } from '@start9labs/shared'

var Convert = require('ansi-to-html')
var convert = new Convert({
  bg: 'transparent',
})

@Component({
  selector: 'logs-window',
  templateUrl: 'logs-window.component.html',
  styleUrls: ['logs-window.component.scss'],
  providers: [DestroyService],
})
export class LogsWindowComponent {
  @ViewChild(IonContent)
  private content?: IonContent

  autoScroll = true

  constructor(
    private readonly api: ApiService,
    private readonly destroy$: DestroyService,
  ) {}

  ngOnInit() {
    from(this.api.followLogs())
      .pipe(
        switchMap(guid =>
          this.api.openLogsWebsocket$(guid).pipe(
            map(log => {
              const container = document.getElementById('container')
              const newLogs = document.getElementById('template')?.cloneNode()

              if (!(newLogs instanceof HTMLElement)) return

              newLogs.innerHTML = this.convertToAnsi(log)

              container?.append(newLogs)

              if (this.autoScroll) {
                setTimeout(() => {
                  this.content?.scrollToBottom(250)
                }, 0)
              }
            }),
          ),
        ),
        takeUntil(this.destroy$),
      )
      .subscribe()
  }

  handleScroll(e: any) {
    if (e.detail.deltaY < 0) this.autoScroll = false
  }

  async handleScrollEnd() {
    const elem = await this.content?.getScrollElement()
    if (elem && elem.scrollHeight - elem.scrollTop - elem.clientHeight < 64) {
      this.autoScroll = true
    }
  }

  private convertToAnsi(log: Log) {
    return `<span style="color: #FFF; font-weight: bold;">${toLocalIsoString(
      new Date(log.timestamp),
    )}</span>&nbsp;&nbsp;${convert.toHtml(log.message)}<br />`
  }
}
