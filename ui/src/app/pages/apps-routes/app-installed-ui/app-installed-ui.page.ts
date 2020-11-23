import { Component, NgZone } from '@angular/core'
import { ApiService } from 'src/app/services/api/api.service'
import { AppModel } from 'src/app/models/app-model'
import { AppAvailablePreview, AppInstalledFull, AppInstalledPreview } from 'src/app/models/app-types'
import { pauseFor } from 'src/app/util/misc.util'
import { PropertySubjectId, initPropertySubject, peekProperties } from 'src/app/util/property-subject.util'
import { Subscription, BehaviorSubject, combineLatest } from 'rxjs'
import { take, tap } from 'rxjs/operators'
import { markAsLoadingDuring$, markAsLoadingDuringP } from 'src/app/services/loader.service'
import { ActivatedRoute } from '@angular/router'
import { ModelPreload } from 'src/app/models/model-preload'

@Component({
  selector: 'app-installed-ui',
  templateUrl: './app-installed-ui.page.html',
  styleUrls: ['./app-installed-ui.page.scss'],
})
export class AppInstalledUiPage {
  private appId: string
  private app: AppInstalledFull
  $loading$ = new BehaviorSubject(true)

  constructor (
    private readonly route: ActivatedRoute,
    private readonly preload: ModelPreload,
  ) { }

  ngOnInit () {
    this.appId = this.route.snapshot.paramMap.get('appId') as string
    markAsLoadingDuring$(this.$loading$, this.preload.appFull(this.appId))
    .pipe(
      tap(app => this.app = peekProperties(app)),
    ).subscribe()
  }

  
}
