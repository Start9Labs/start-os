import {
  ChangeDetectionStrategy,
  Component,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { take } from 'rxjs/operators'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { T } from '@start9labs/start-sdk'

@Component({
  selector: 'backing-up',
  templateUrl: './backing-up.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BackingUpComponent {
  readonly pkgs$ = this.patch.watch$('packageData').pipe(take(1))
  readonly backupProgress$ = this.patch.watch$(
    'serverInfo',
    'statusInfo',
    'backupProgress',
  )

  constructor(private readonly patch: PatchDB<DataModel>) {}
}

@Pipe({
  name: 'pkgMainStatus',
})
export class PkgMainStatusPipe implements PipeTransform {
  transform(pkgId: string): Observable<T.MainStatus['status']> {
    return this.patch.watch$('packageData', pkgId, 'status', 'main', 'status')
  }

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
