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
  ): Observable<Record<string, T.NamedHealthCheckResult | null> | null> {
    return this.patch.watch$('packageData', manifest.id, 'status').pipe(
      map(status => {
        return status.main === 'running' && !isEmptyObject(status.health)
          ? status.health
          : null
      }),
      startWith(null),
    )
  }
}
