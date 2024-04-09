import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map, Observable, shareReplay } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Injectable({
  providedIn: 'root',
})
export class ServicesService extends Observable<readonly PackageDataEntry[]> {
  private readonly services$ = inject(PatchDB<DataModel>)
    .watch$('packageData')
    .pipe(
      map(pkgs =>
        Object.values(pkgs).sort((a, b) =>
          getManifest(b).title.toLowerCase() >
          getManifest(a).title.toLowerCase()
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
