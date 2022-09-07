import {
  ChangeDetectionStrategy,
  Component,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { filter, take } from 'rxjs/operators'
import {
  DataModel,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'

@Component({
  selector: 'backing-up',
  templateUrl: './backing-up.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BackingUpComponent {
  readonly pkgs$ = this.patch
    .watch$('package-data')
    .pipe(filter(Boolean), take(1))
  readonly backupProgress$ = this.patch.watch$(
    'server-info',
    'status-info',
    'backup-progress',
  )

  PackageMainStatus = PackageMainStatus

  constructor(private readonly patch: PatchDB<DataModel>) {}
}

@Pipe({
  name: 'pkgMainStatus',
})
export class PkgMainStatusPipe implements PipeTransform {
  transform(pkgId: string): Observable<PackageMainStatus> {
    return this.patch.watch$(
      'package-data',
      pkgId,
      'installed',
      'status',
      'main',
      'status',
    )
  }

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
