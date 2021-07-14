import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { IonContent } from '@ionic/angular'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-logs',
  templateUrl: './app-logs.page.html',
  styleUrls: ['./app-logs.page.scss'],
})
export class AppLogsPage {
  @ViewChild(IonContent, { static: false }) private content: IonContent
  pkgId: string
  logs = ''

  constructor (
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly apiService: ApiService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.getLogs()
  }

  async getLogs () {
    this.logs = ''

    try {
      const logs = await this.apiService.getPackageLogs({ id: this.pkgId })
      this.logs = logs.map(l => `${l.timestamp} ${l.log}`).join('\n\n')
      setTimeout(async () => await this.content.scrollToBottom(100), 200)
    } catch (e) {
      console.error(e)
      this.errToast.present(e.message)
    }
  }
}
