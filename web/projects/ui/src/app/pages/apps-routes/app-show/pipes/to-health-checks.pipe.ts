import { Pipe, PipeTransform } from '@angular/core'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { isEmptyObject } from '@start9labs/shared'
import { map, startWith } from 'rxjs/operators'
import { PatchDB } from 'patch-db-client'
import { Observable } from 'rxjs'
import { T } from '@start9labs/start-sdk'

@Pipe({
  name: 'toHealthChecks',
})
export class ToHealthChecksPipe implements PipeTransform {
  constructor(private readonly patch: PatchDB<DataModel>) {}

  transform(
    manifest: T.Manifest,
  ): Observable<Record<string, T.HealthCheckResult | null> | null> {
    return this.patch.watch$('packageData', manifest.id, 'status', 'main').pipe(
      map(main => {
        return main.status === 'running' && !isEmptyObject(main.health)
          ? main.health
          : null
      }),
      startWith(null),
    )
  }
}
