import { Component, Input, OnInit } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { Cleanup } from 'src/app/util/cleanup'
import { AppMetrics, AppMetricString, flattenMetrics } from 'src/app/util/metrics.util'
import { traceWheel } from 'src/app/util/misc.util'
import { copyToClipboard } from 'src/app/util/web.util'

@Component({
  selector: 'app-service-ui-menu',
  templateUrl: './service-ui-menu.component.html',
  styleUrls: ['./service-ui-menu.component.scss'],
})
export class ServiceUiMenuComponent implements OnInit {
  @Input()
  properties$: Observable<AppMetrics>
  menuItems$: Observable<[string, AppMetricString][]>

  toast: HTMLIonToastElement

  constructor (private readonly toastCtrl: ToastController) { }

  ngOnInit () {
    this.menuItems$ = this.properties$.pipe(
      map(
        ps => Object.entries(flattenMetrics(ps)).filter(([k, v]) => v.copyable).sort(
          ([k1, v], [k2, v2]) => k1 < k2 ? -1 : 1,
        ),
      ),
      traceWheel('props'),
    )
  }

  ionViewWillLeave () {
    this.clearToast()
  }

  async copy (item: [string, AppMetricString], ev: Event) {
    ev.stopPropagation()
    let message = ''
    await copyToClipboard(item[1].value).then(success => { message = success ? 'copied to clipboard!' :  'failed to copy'})

    await this.clearToast()
    this.toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
      cssClass: 'notification-toast',
    })
    await this.toast.present()
  }

  async autoFill (item: [string, AppMetricString]) {
    console.log('autofill', item)

    await this.clearToast()
    this.toast = await this.toastCtrl.create({
      header: 'Unable to auto-fill the selected item on this page. Copy and paste the information instead.',
      position: 'bottom',
      cssClass: 'notification-toast',
    })
    await this.toast.present()
  }

  async clearToast () {
    if (this.toast) { this.toast.dismiss(); this.toast = undefined }
  }
}
