import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { PatchDB } from 'patch-db-client'
import { tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { NavigationService } from '../../components/navigation/navigation.service'
import { toRouterLink } from '../../utils/to-router-link'

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

  readonly service$ = this.patch
    .watch$('package-data', getPkgId(this.route))
    .pipe(
      tap(pkg => {
        // if package disappears, navigate to list page
        if (!pkg) {
          this.router.navigate(['..'], { relativeTo: this.route })
        } else {
          this.navigation.addTab({
            icon: pkg.icon,
            title: pkg.manifest.title,
            isService: true,
            routerLink: toRouterLink(pkg.manifest.id),
          })
        }
      }),
    )
}
