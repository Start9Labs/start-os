import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getPkgId } from '@start9labs/shared'

@Component({
  selector: 'app-interface',
  templateUrl: './app-interface.page.html',
  styleUrls: ['./app-interface.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppInterfacePage {
  readonly pkgId = getPkgId(this.route)
  readonly interfaceId = this.route.snapshot.paramMap.get('interfaceId')!

  readonly interfaceInfo$ = this.patch.watch$(
    'package-data',
    this.pkgId,
    'installed',
    'interfaceInfo',
    this.interfaceId,
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}
