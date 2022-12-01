import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
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
    startWith([]),
    pairwise(),
    filter(([prev, next]) => {
      const length = next.length
      return !length || prev.length !== length
    }),
    map(([_, pkgs]) =>
      pkgs.sort((a, b) =>
        b.manifest.title.toLowerCase() > a.manifest.title.toLowerCase()
          ? -1
          : 1,
      ),
    ),
  )

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
