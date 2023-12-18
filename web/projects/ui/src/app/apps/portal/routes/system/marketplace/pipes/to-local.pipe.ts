import { inject, Pipe, PipeTransform } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { filter, Observable } from 'rxjs'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Pipe({
  name: 'toLocal',
  standalone: true,
})
export class ToLocalPipe implements PipeTransform {
  private readonly patch = inject(PatchDB<DataModel>)

  transform(id: string): Observable<PackageDataEntry> {
    return this.patch.watch$('package-data', id).pipe(filter(Boolean))
  }
}
