import { Injectable } from '@angular/core'
import { ActivatedRouteSnapshot, Resolve } from '@angular/router'
import { Observable } from 'rxjs'
import { first } from 'rxjs/operators'

import { PatchDbService } from '../patch-db/patch-db.service'
import { PackageDataEntry } from '../patch-db/data-model'

@Injectable({ providedIn: 'root' })
export class PkgResolver implements Resolve<PackageDataEntry> {
  constructor(private readonly patch: PatchDbService) {}

  resolve({ paramMap }: ActivatedRouteSnapshot): Observable<PackageDataEntry> {
    const id = paramMap.get('pkgId') || ''

    return this.patch.watch$('package-data', id).pipe(first())
  }
}
