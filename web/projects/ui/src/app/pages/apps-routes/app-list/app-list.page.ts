import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { filter, map, pairwise, startWith } from 'rxjs/operators'
import { getManifest } from 'src/app/util/get-package-data'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppListPage {
  readonly pkgs$ = this.patch.watch$('packageData').pipe(
    map(pkgs => Object.values(pkgs)),
    startWith([]),
    pairwise(),
    filter(([prev, next]) => {
      const length = next.length
      return !length || prev.length !== length
    }),
    map(([_, pkgs]) =>
      pkgs.sort((a, b) =>
        getManifest(b).title.toLowerCase() > getManifest(a).title.toLowerCase()
          ? -1
          : 1,
      ),
    ),
  )

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
