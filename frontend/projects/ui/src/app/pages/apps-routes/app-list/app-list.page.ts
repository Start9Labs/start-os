import { Component } from '@angular/core'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { Observable } from 'rxjs'
import { filter, map, switchMap, take, takeUntil, tap } from 'rxjs/operators'
import { isEmptyObject, exists, DestroyService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { parseDataModel, RecoveredInfo } from 'src/app/util/parse-data-model'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
  providers: [DestroyService],
})
export class AppListPage {
  loading = true
  pkgs: readonly PackageDataEntry[] = []
  recoveredPkgs: readonly RecoveredInfo[] = []
  reordering = false

  constructor(
    private readonly api: ApiService,
    private readonly destroy$: DestroyService,
    private readonly patch: PatchDbService,
  ) {}

  get empty(): boolean {
    return !this.pkgs.length && isEmptyObject(this.recoveredPkgs)
  }

  ngOnInit() {
    this.patch
      .watch$()
      .pipe(
        filter(data => exists(data) && !isEmptyObject(data)),
        take(1),
        map(parseDataModel),
        tap(({ pkgs, recoveredPkgs }) => {
          this.loading = false
          this.pkgs = pkgs
          this.recoveredPkgs = recoveredPkgs
        }),
        switchMap(() => this.watchNewlyRecovered()),
        takeUntil(this.destroy$),
      )
      .subscribe()
  }

  onReordering(reordering: boolean): void {
    if (!reordering) {
      this.setOrder()
    }

    this.reordering = reordering
  }

  deleteRecovered(rec: RecoveredInfo): void {
    this.recoveredPkgs = this.recoveredPkgs.filter(item => item !== rec)
  }

  private watchNewlyRecovered(): Observable<unknown> {
    return this.patch.watch$('package-data').pipe(
      filter(pkgs => !!pkgs && Object.keys(pkgs).length !== this.pkgs.length),
      tap(pkgs => {
        const ids = Object.keys(pkgs)
        const newIds = ids.filter(
          id => !this.pkgs.find(pkg => pkg.manifest.id === id),
        )

        // remove uninstalled
        const filtered = this.pkgs.filter(pkg => ids.includes(pkg.manifest.id))

        // add new entry to beginning of array
        const added = newIds.map(id => pkgs[id])

        this.pkgs = [...added, ...filtered]
        this.recoveredPkgs = this.recoveredPkgs.filter(rec => !pkgs[rec.id])
      }),
    )
  }

  private setOrder(): void {
    const order = this.pkgs.map(pkg => pkg.manifest.id)
    this.api.setDbValue({ pointer: '/pkg-order', value: order })
  }
}
