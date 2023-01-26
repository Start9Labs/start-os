import { Inject, Injectable } from '@angular/core'
import { WINDOW } from '@ng-web-apis/common'
import { PatchDB } from 'patch-db-client'
import { BehaviorSubject } from 'rxjs'
import { ApiService } from './api/embassy-api.service'
import { DataModel } from './patch-db/data-model'
import { filter, take } from 'rxjs/operators'

@Injectable({
  providedIn: 'root',
})
export class ThemeSwitcherService extends BehaviorSubject<string> {
  constructor(
    private readonly patch: PatchDB<DataModel>,
    private readonly embassyApi: ApiService,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {
    super('Dark')

    this.patch
      .watch$('ui', 'theme')
      .pipe(
        take(1),
        filter(theme => !!theme && theme !== this.value),
      )
      .subscribe(theme => {
        this.next(theme)
      })
  }

  override next(currentTheme: string): void {
    this.embassyApi.setDbValue(['theme'], currentTheme)
    this.windowRef.document.body.setAttribute('data-theme', currentTheme)
    super.next(currentTheme)
  }
}
