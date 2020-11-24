import { Component, ElementRef, ViewChild } from '@angular/core'
import { AppInstalledFull } from 'src/app/models/app-types'
import { PropertySubject } from 'src/app/util/property-subject.util'
import { BehaviorSubject, EMPTY, from, Observable, of } from 'rxjs'
import { catchError, concatMap, delay, map, switchMap, tap } from 'rxjs/operators'
import { markAsLoadingDuring$ } from 'src/app/services/loader.service'
import { ActivatedRoute } from '@angular/router'
import { ModelPreload } from 'src/app/models/model-preload'
import { PopoverController } from '@ionic/angular'
import { ServiceUiMenuComponent } from 'src/app/components/service-ui-menu/service-ui-menu.component'
import { AppMetrics } from 'src/app/util/metrics.util'
import { ApiService } from 'src/app/services/api/api.service'
import { AppModel } from 'src/app/models/app-model'
import { Cleanup } from 'src/app/util/cleanup'

@Component({
  selector: 'app-installed-ui',
  templateUrl: './app-installed-ui.page.html',
  styleUrls: ['./app-installed-ui.page.scss'],
})
export class AppInstalledUiPage extends Cleanup {
  private appId: string
  $app$: PropertySubject<AppInstalledFull>
  error: string = undefined
  $properties$: BehaviorSubject<AppMetrics> = new BehaviorSubject({ })
  $loading$ = new BehaviorSubject(true)

  constructor (
    private readonly route: ActivatedRoute,
    private readonly preload: ModelPreload,
    private readonly popover: PopoverController,
    private readonly apiService: ApiService,
    private readonly appModel: AppModel,
  ) {  super() }

  ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string
    this.cleanup(
      markAsLoadingDuring$(this.$loading$,
        this.preload.appFull(this.appId)
        .pipe(
          tap(app => this.$app$ = app),
          concatMap(() => this.getMetrics()),
        ),
      ).pipe(
        concatMap(() =>
          this.appModel.watchForRunning(this.appId).pipe(
            () => this.getMetrics(),
          ),
        ),
      ).subscribe(),
    )
  }

  pop: HTMLIonPopoverElement
  async presentPopoverMenu (ev: Event) {
    if (this.pop) await this.pop.dismiss()
    this.pop = await this.popover.create({
      component: ServiceUiMenuComponent,
      componentProps: {
        properties$: this.$properties$,
        iFrame: document.getElementById(`${this.appId}-ui`),
      },
      showBackdrop: true,
      backdropDismiss: true,
      event: ev,
      cssClass: 'ui-menu',
    })

    await this.pop.present()
  }

  ionViewDidLeave () {
    if (this.pop) this.pop.dismiss()
  }

  getMetrics (): Observable<void> {
    return from(this.apiService.getAppMetrics(this.appId)).pipe(
      tap(() => this.error = undefined),
      map(metrics => this.$properties$.next(metrics)),
      catchError(e => {
        this.error = e.message
        return of(console.error(`Exception in metrics polling`, e))
      }),
    )
  }

}
