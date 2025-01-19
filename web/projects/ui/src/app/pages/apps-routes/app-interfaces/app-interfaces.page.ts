import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { combineLatest, map } from 'rxjs'
import { getAddresses } from 'src/app/components/interface-info/interface-info.component'

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)

  private readonly serviceInterfaces$ = this.patch.watch$(
    'packageData',
    this.pkgId,
    'serviceInterfaces',
  )
  private readonly hosts$ = this.patch.watch$(
    'packageData',
    this.pkgId,
    'hosts',
  )

  readonly serviceInterfacesWithHostInfo$ = combineLatest([
    this.serviceInterfaces$,
    this.hosts$,
  ]).pipe(
    map(([interfaces, hosts]) => {
      const sorted = Object.values(interfaces)
        .sort(iface =>
          iface.name.toLowerCase() > iface.name.toLowerCase() ? -1 : 1,
        )
        .map(iface => {
          const host = hosts[iface.addressInfo.hostId]
          return {
            ...iface,
            public: host.bindings[iface.addressInfo.internalPort].net.public,
            addresses: getAddresses(iface, host),
          }
        })

      return {
        ui: sorted.filter(val => val.type === 'ui'),
        api: sorted.filter(val => val.type === 'api'),
        p2p: sorted.filter(val => val.type === 'p2p'),
      }
    }),
  )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}
