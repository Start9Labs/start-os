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
      .pipe(take(1), filter(Boolean))
      .subscribe(theme => {
        this.updateTheme(theme)
      })
  }

  override next(theme: string): void {
    this.embassyApi.setDbValue(['theme'], theme)
    this.updateTheme(theme)
  }

  private updateTheme(theme: string): void {
    this.windowRef.document.body.setAttribute('data-theme', theme)
    super.next(theme)
  }
}
