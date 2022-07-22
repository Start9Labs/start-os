import {
  ChangeDetectionStrategy,
  Component,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { take } from 'rxjs/operators'
import { PackageMainStatus } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'

@Component({
  selector: 'backing-up',
  templateUrl: './backing-up.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BackingUpComponent {
  readonly pkgs$ = this.patch.watch$('package-data').pipe(take(1))
  readonly backupProgress$ = this.patch.watch$(
    'server-info',
    'status-info',
    'backup-progress',
  )

  PackageMainStatus = PackageMainStatus

  constructor(private readonly patch: PatchDbService) {}
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

  constructor(private readonly patch: PatchDbService) {}
}
