import { inject, Injectable } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map, Observable, shareReplay } from 'rxjs'
import {
  DataModel,
  InstalledState,
  PackageDataEntry,
  UpdatingState,
} from 'src/app/services/patch-db/data-model'
import { isInstalled, isUpdating } from 'src/app/utils/get-package-data'

@Injectable({
  providedIn: 'root',
})
export class LocalPackagesService extends Observable<
  Record<string, PackageDataEntry<InstalledState | UpdatingState>>
> {
  private readonly stream$ = inject<PatchDB<DataModel>>(PatchDB)
    .watch$('packageData')
    .pipe(
      map(pkgs =>
        Object.entries(pkgs).reduce(
          (acc, [id, val]) =>
            isInstalled(val) || isUpdating(val) ? { ...acc, [id]: val } : acc,
          {},
        ),
      ),
      shareReplay({ bufferSize: 1, refCount: true }),
    )

  constructor() {
    super(subscriber => this.stream$.subscribe(subscriber))
  }
}
