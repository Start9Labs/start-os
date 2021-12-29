import { Inject, Pipe, PipeTransform } from '@angular/core'
import {
  HealthCheckResult,
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { exists, isEmptyObject } from 'src/app/util/misc.util'
import { filter, map, startWith } from 'rxjs/operators'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { Observable } from 'rxjs'

@Pipe({
  name: 'toHealthChecks',
})
export class ToHealthChecksPipe implements PipeTransform {
  constructor(private readonly patch: PatchDbService) {}

  transform(
    pkg: PackageDataEntry,
  ): Observable<Record<string, HealthCheckResult | null>> | null {
    const healthChecks = Object.keys(pkg.manifest['health-checks']).reduce(
      (obj, key) => ({ ...obj, [key]: null }),
      {},
    )

    const healthChecks$ = this.patch
      .watch$('package-data', pkg.manifest.id, 'installed', 'status', 'main')
      .pipe(
        filter(obj => exists(obj)),
        map(main =>
          // Question: is this ok or do we have to use Object.keys
          // to maintain order and the keys initially present in pkg?
          main.status === PackageMainStatus.Running
            ? main.health
            : healthChecks,
        ),
        startWith(healthChecks),
      )

    return isEmptyObject(healthChecks) ? null : healthChecks$
  }
}
