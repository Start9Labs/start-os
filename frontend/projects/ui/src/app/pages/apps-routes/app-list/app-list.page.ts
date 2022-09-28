import { Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { filter, takeUntil, tap } from 'rxjs/operators'
import { DestroyService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'app-list',
  templateUrl: './app-list.page.html',
  styleUrls: ['./app-list.page.scss'],
  providers: [DestroyService],
})
export class AppListPage {
  loading = true
  pkgs: readonly PackageDataEntry[] = []

  constructor(
    private readonly api: ApiService,
    private readonly destroy$: DestroyService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get empty(): boolean {
    return !this.pkgs.length
  }

  ngOnInit() {
    this.patch
      .watch$('package-data')
      .pipe(
        filter(pkgs => Object.keys(pkgs).length !== this.pkgs.length),
        tap(pkgs => {
          this.loading = false
          this.pkgs = Object.values(pkgs).sort((a, b) =>
            b.manifest.title > a.manifest.title ? -1 : 1,
          )
        }),
        takeUntil(this.destroy$),
      )
      .subscribe()
  }
}
