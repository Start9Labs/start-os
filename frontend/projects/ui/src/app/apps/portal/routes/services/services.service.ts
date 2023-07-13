import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { filter, map, Observable, pairwise, shareReplay, startWith } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'

@Injectable({
  providedIn: 'root',
})
export class ServicesService extends Observable<readonly PackageDataEntry[]> {
  private readonly services$ = inject<PatchDB<DataModel>>(PatchDB)
    .watch$('package-data')
    .pipe(
      map(pkgs => Object.values(pkgs)),
      startWith([]),
      pairwise(),
      filter(([prev, next]) => {
        const length = next.length
        return !length || prev.length !== length
      }),
      map(([_, pkgs]) =>
        pkgs.sort((a, b) =>
          b.manifest.title.toLowerCase() > a.manifest.title.toLowerCase()
            ? -1
            : 1,
        ),
      ),
      shareReplay(1),
    )

  constructor() {
    super(subscriber => this.services$.subscribe(subscriber))
  }
}
