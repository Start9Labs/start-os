import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { first, shareReplay } from 'rxjs'

@Injectable({
  providedIn: 'root',
})
export class DesktopService {
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly desktop$ = this.patch.watch$('ui', 'desktop').pipe(shareReplay(1))

  add(id: string) {
    this.desktop$
      .pipe(first())
      .subscribe(desktop => this.save(desktop.concat(id)))
  }

  remove(id: string) {
    this.desktop$
      .pipe(first())
      .subscribe(desktop => this.save(desktop.filter(x => x !== id)))
  }

  save(ids: readonly string[]) {
    this.api.setDbValue(['desktop'], ids)
  }
}
