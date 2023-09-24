import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { getPkgId, isEmptyObject } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { map, Observable, tap } from 'rxjs'
import {
  DataModel,
  HealthCheckResult,
  PackageDataEntry,
  PackageState,
  ServiceOutboundProxy,
} from 'src/app/services/patch-db/data-model'
import {
  PackageStatus,
  PrimaryRendering,
  PrimaryStatus,
  StatusRendering,
} from 'src/app/services/pkg-status-rendering.service'
import { ConnectionService } from 'src/app/services/connection.service'
import { NavigationService } from '../../components/navigation/navigation.service'
import { toRouterLink } from '../../utils/to-router-link'

const STATES = [
  PackageState.Installing,
  PackageState.Updating,
  PackageState.Restoring,
]

@Component({
  templateUrl: 'service.component.html',
  styleUrls: ['service.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ServiceComponent {
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly pkgId = getPkgId(this.route)

  readonly connected$ = inject(ConnectionService).connected$

  readonly service$ = this.patch.watch$('package-data', this.pkgId).pipe(
    tap(pkg => {
      // if package disappears, navigate to list page
      if (!pkg) {
        this.router.navigate(['..'], { relativeTo: this.route })
      } else {
        this.navigation.addTab({
          icon: pkg.icon,
          title: pkg.manifest.title,
          routerLink: toRouterLink(pkg.manifest.id),
        })
      }
    }),
  )

  readonly health$: Observable<HealthCheckResult[] | null> = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'status', 'main')
    .pipe(
      map(main =>
        main.status !== 'running' || isEmptyObject(main.health)
          ? null
          : Object.values(main.health),
      ),
    )

  getRendering({ primary }: PackageStatus): StatusRendering {
    return PrimaryRendering[primary]
  }

  isInstalled({ state }: PackageDataEntry): boolean {
    return state === PackageState.Installed
  }

  isRunning({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.Running
  }

  isBackingUp({ primary }: PackageStatus): boolean {
    return primary === PrimaryStatus.BackingUp
  }

  showProgress({ state }: PackageDataEntry): boolean {
    return STATES.includes(state)
  }

  getProxy(proxy?: ServiceOutboundProxy): string {
    switch (proxy) {
      case 'primary':
        return 'System Primary'
      case 'mirror':
        return 'Mirror P2P'
      default:
        return proxy?.proxyId || 'None'
    }
  }
}
