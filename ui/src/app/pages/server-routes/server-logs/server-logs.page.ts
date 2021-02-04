import { Component, ViewChild } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { IonContent } from '@ionic/angular'
import { pauseFor } from 'src/app/util/misc.util'
import { markAsLoadingDuringP } from 'src/app/services/loader.service'
import { BehaviorSubject } from 'rxjs'

@Component({
  selector: 'server-logs',
  templateUrl: './server-logs.page.html',
  styleUrls: ['./server-logs.page.scss'],
})
export class ServerLogsPage {
  @ViewChild(IonContent, { static: false }) private content: IonContent
  $loading$ = new BehaviorSubject(true)
  error = ''
  logs: string

  constructor (
    private readonly apiService: ApiService,
  ) { }

  async ngOnInit () {
    markAsLoadingDuringP(this.$loading$, Promise.all([
      this.getLogs(),
      pauseFor(600),
    ]))
  }

  async getLogs () {
    this.logs = ''
    this.$loading$.next(true)
    try {
      this.logs = await this.apiService.getServerLogs()
      this.error = ''
      setTimeout(async () => await this.content.scrollToBottom(100), 200)
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.$loading$.next(false)
    }
  }
}
