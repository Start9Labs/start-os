import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService, isRpcFailure, isRpcSuccess } from 'src/app/services/api/api.service'
import { BehaviorSubject } from 'rxjs'
import { AlertController, IonicSafeString } from '@ionic/angular'
import { ModelPreload } from 'src/app/models/model-preload'
import { LoaderService, markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { ServiceAction, AppInstalledFull } from 'src/app/models/app-types'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { map } from 'rxjs/operators'
import { Cleanup } from 'src/app/util/cleanup'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
})
export class AppActionsPage extends Cleanup {
  error = ''
  $loading$ = new BehaviorSubject(true)
  appId: string
  app: PropertySubject<AppInstalledFull>

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private readonly alertCtrl: AlertController,
    private readonly preload: ModelPreload,
    private readonly loaderService: LoaderService,
  ) { super() }

  ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId')

    markAsLoadingDuring$(this.$loading$, this.preload.appFull(this.appId)).pipe(
      map(app => this.app = app),
    ).subscribe( { error: e => this.error = e.message } )
  }

  async handleAction (action: ServiceAction) {
    const alert = await this.alertCtrl.create({
      backdropDismiss: false,
      header: `Are you sure you want to ${action.name}?`,
      message: new IonicSafeString(`
        <p>${action.description}</p>
      `),
      buttons: [
        {
          text: 'Cancel',
          role: 'cancel',
        },
        {
          text: `Go`,
          handler: () => {
            this.executeAction(action)

          },
        },
      ],
    })
    await alert.present()
  }

  private async executeAction (action: ServiceAction) {
    const res = await this.loaderService.displayDuringP(
      this.apiService.serviceAction(this.appId, action),
    )
    if (isRpcFailure(res)) {
      const successAlert = await this.alertCtrl.create({
        header: `${action.name} failed with code ${res.error.code}`,
        message: new IonicSafeString(`
          <p>${res.error.message}</p>
        `),
        cssClass: 'alert-error-message',
      })
      return await successAlert.present()
    }

    if (isRpcSuccess(res)) {
      const successAlert = await this.alertCtrl.create({
        header: `${action.name}`,
        message: new IonicSafeString(`${res.result}`),
      })
      return await successAlert.present()
    }
  }
}
