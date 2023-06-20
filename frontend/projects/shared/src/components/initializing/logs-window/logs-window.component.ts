import { Component, ViewChild } from '@angular/core'
import { IonContent } from '@ionic/angular'
import { map, takeUntil } from 'rxjs'
import { TuiDestroyService } from '@taiga-ui/cdk'
import { SetupLogsService } from '../../../services/setup-logs.service'
import { Log } from '../../../types/api'
import { toLocalIsoString } from '../../../util/to-local-iso-string'

import Convert from 'ansi-to-html'
const convert = new Convert({
  bg: 'transparent',
})

@Component({
  selector: 'logs-window',
  templateUrl: 'logs-window.component.html',
  styleUrls: ['logs-window.component.scss'],
  providers: [TuiDestroyService],
})
export class LogsWindowComponent {
  @ViewChild(IonContent)
  private content?: IonContent

  autoScroll = true

  constructor(
    private readonly logs: SetupLogsService,
    private readonly destroy$: TuiDestroyService,
  ) {}

  ngOnInit() {
    this.logs
      .pipe(
        map(log => this.convertToAnsi(log)),
        takeUntil(this.destroy$),
      )
      .subscribe(innerHTML => {
        const container = document.getElementById('container')
        const newLogs = document.getElementById('template')?.cloneNode()

        if (!(newLogs instanceof HTMLElement)) return

        newLogs.innerHTML = innerHTML
        container?.append(newLogs)

        if (this.autoScroll) {
          setTimeout(() => this.content?.scrollToBottom(250))
        }
      })
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
