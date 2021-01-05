import { Component, Input, OnInit } from '@angular/core'
import { ToastController, PopoverController, ModalController } from '@ionic/angular'
import { Observable } from 'rxjs'
import { map } from 'rxjs/operators'
import { AppLogsPage } from 'src/app/pages/apps-routes/app-logs/app-logs.page'
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
  @Input()
  appId: string
  @Input()
  quit: () => void

  menuItems$: Observable<[string, AppMetricString][]>

  toast: HTMLIonToastElement

  constructor (
    private readonly toastCtrl: ToastController,
    private readonly popover: PopoverController,
    private readonly modalCtrl: ModalController) { }

  ngOnInit () {
    this.menuItems$ = this.properties$.pipe(
      map(
        ps => Object.entries(flattenMetrics(ps))
          .filter(([_, v]) => v.copyable)
          .sort(([k1, _], [k2, __]) => k1 < k2 ? 1 : -1),
      ),
      traceWheel('props'),
    )
  }

  ionViewWillLeave () {
    this.clearToast()
  }

  async logs() {
    const m = await this.modalCtrl.create({
      component: AppLogsPage,
      componentProps: {
        isModal: true,
        appId: this.appId
      }
     })
    await m.present()
    return this.popover.dismiss()
  }

  async copy (item: [string, AppMetricString]) {
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


  // Need JS injection on inner services for this to work
  // async autoFill (item: [string, AppMetricString], ev: Event) {
  //   ev.stopPropagation()

  //   await this.clearToast()
  //   this.toast = await this.toastCtrl.create({
  //     header: 'Unable to auto-fill the selected item on this page. Copy and paste the information instead.',
  //     position: 'bottom',
  //     cssClass: 'notification-toast',
  //   })
  //   await this.toast.present()
  // }

  async clearToast () {
    if (this.toast) { this.toast.dismiss(); this.toast = undefined }
  }
}

// function getSelections (iframe: HTMLElement, a: [string, AppMetricString]): [string, AppMetricString & { selection?: NodeList }] {
//   if (!a[1].autofillSelector) return a
//   const selection = iframe.
// }