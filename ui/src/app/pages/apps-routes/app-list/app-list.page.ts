import { Component } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { filter, map, switchMapTo, take, takeUntil, tap } from 'rxjs/operators'
import { isEmptyObject, exists } from 'src/app/util/misc.util'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { parseDataModel, RecoveredInfo } from 'src/app/util/parse-data-model'
import { DestroyService } from 'src/app/services/destroy.service'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
  providers: [DestroyService],
})
export class AppListPage {
  pkgs: readonly PackageDataEntry[] = []
  recoveredPkgs: readonly RecoveredInfo[] = []
  order: readonly string[] = []
  reordering = false

  constructor (
    private readonly api: ApiService,
    private readonly destroy$: DestroyService,
    public readonly patch: PatchDbService,
  ) { }

  get empty (): boolean {
    return !this.pkgs.length && isEmptyObject(this.recoveredPkgs)
  }

  ngOnInit () {
    this.patch
      .watch$()
      .pipe(
        filter((data) => exists(data) && !isEmptyObject(data)),
        take(1),
        map(parseDataModel),
        tap(({ order, pkgs, recoveredPkgs }) => {
          this.pkgs = pkgs
          this.recoveredPkgs = recoveredPkgs
          this.order = order

          // set order in UI DB if there were unknown packages
          if (order.length < pkgs.length) {
            this.setOrder()
          }
        }),
        switchMapTo(this.watchNewlyRecovered()),
        takeUntil(this.destroy$),
      )
      .subscribe()
  }

  onReordering (reordering: boolean): void {
    if (!reordering) {
      this.setOrder()
    }

    this.reordering = reordering
  }

  deleteRecovered (rec: RecoveredInfo): void {
    this.recoveredPkgs = this.recoveredPkgs.filter((item) => item !== rec)
  }

  private watchNewlyRecovered (): Observable<unknown> {
    return this.patch.watch$('package-data').pipe(
      filter((pkgs) => !!pkgs && Object.keys(pkgs).length !== this.pkgs.length),
      tap((pkgs) => {
        const ids = Object.keys(pkgs)
        const newIds = ids.filter(
          (id) => !this.pkgs.find((pkg) => pkg.manifest.id === id),
        )

        // remove uninstalled
        const filtered = this.pkgs.filter((pkg) =>
          ids.includes(pkg.manifest.id),
        )

        // add new entry to beginning of array
        const added = newIds.map((id) => pkgs[id])

        this.pkgs = [...added, ...filtered]
        this.recoveredPkgs = this.recoveredPkgs.filter((rec) => !pkgs[rec.id])
      }),
    )
  }

  private setOrder (): void {
    this.order = this.pkgs.map((pkg) => pkg.manifest.id)
    this.api.setDbValue({ pointer: '/pkg-order', value: this.order })
  }
}
