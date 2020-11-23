import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { IonContent } from '@ionic/angular'
import { pauseFor } from 'src/app/util/misc.util'
import { markAsLoadingDuringP } from 'src/app/services/loader.service'
import { BehaviorSubject } from 'rxjs'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  @ViewChild(IonContent, { static: false }) private content: IonContent
  $loading$ = new BehaviorSubject(true)
  error = ''
  appId: string
  logs: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
  ) { }

  async ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string

    markAsLoadingDuringP(this.$loading$, Promise.all([
      this.getLogs(),
      pauseFor(600),
    ]))
  }

  async getLogs () {
    this.logs = ''
    this.$loading$.next(true)
    try {
      const logs = await this.apiService.getAppLogs(this.appId)
      this.logs = logs.join('\n\n')
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
