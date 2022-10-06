import { Pipe, PipeTransform } from '@angular/core'
import {
  DataModel,
  HealthCheckResult,
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { isEmptyObject } from '@start9labs/shared'
import { map, startWith } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { Observable } from 'rxjs'

@Pipe({
  name: 'toHealthChecks',
})
export class ToHealthChecksPipe implements PipeTransform {
  constructor(private readonly patch: PatchDB<DataModel>) {}

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
        map(main => {
          // Question: is this ok or do we have to use Object.keys
          // to maintain order and the keys initially present in pkg?
          return main.status === PackageMainStatus.Running &&
            !isEmptyObject(main.health)
            ? main.health
            : healthChecks
        }),
        startWith(healthChecks),
      )

    return isEmptyObject(healthChecks) ? null : healthChecks$
  }
}
