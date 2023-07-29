import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { InterfaceInfo, DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)
  readonly interfaceInfo$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'interfaceInfo')
    .pipe(map(interfaceInfo => Object.values(interfaceInfo)))

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input()
  interfaceInfo!: InterfaceInfo
}
