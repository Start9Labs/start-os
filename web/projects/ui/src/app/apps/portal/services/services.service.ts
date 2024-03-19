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
  private readonly services$ = inject(PatchDB<DataModel>)
    .watch$('package-data')
    .pipe(
      map(pkgs =>
        Object.values(pkgs).sort((a, b) =>
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
