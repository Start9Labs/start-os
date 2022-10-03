import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { filter, map, pairwise, startWith } from 'rxjs/operators'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPage {
  readonly pkgs$ = this.patch.watch$('package-data').pipe(
    map(pkgs => Object.values(pkgs)),
    startWith<PackageDataEntry[]>([]),
    pairwise(),
    filter(([prev, next]) => prev.length !== next.length),
    map(([_, pkgs]) => {
      return pkgs.sort((a, b) => (b.manifest.title > a.manifest.title ? -1 : 1))
    }),
  )

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
