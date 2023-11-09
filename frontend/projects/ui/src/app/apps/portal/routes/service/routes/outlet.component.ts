import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, Router, RouterModule } from '@angular/router'
import { TuiSvgModule } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, filter, map, switchMap, tap } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { toRouterLink } from '../../../utils/to-router-link'
import { NavigationService } from '../../../services/navigation.service'

@Component({
  template: `
    <a
      *ngIf="service$ | async as service"
      routerLinkActive="_current"
      [routerLinkActiveOptions]="{ exact: true }"
      [routerLink]="getLink(service.manifest.id)"
      (isActiveChange)="onActive(service, $event)"
    >
      <tui-svg src="tuiIconChevronLeftLarge" />
      {{ service.manifest.title }}
    </a>
    <router-outlet></router-outlet>
  `,
  styles: [
    `
      a {
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        margin: 1rem 0;
        font-size: 1rem;
        color: var(--tui-text-01);
      }

      ._current {
        display: none;
      }
    `,
  ],
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, RouterModule, TuiSvgModule],
})
export class ServiceOutletComponent {
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)
  private readonly navigation = inject(NavigationService)

  readonly service$ = this.router.events.pipe(
    map(() => this.route.firstChild?.snapshot.paramMap?.get('pkgId')),
    filter(Boolean),
    distinctUntilChanged(),
    switchMap(id => this.patch.watch$('package-data', id)),
    tap(pkg => {
      // if package disappears, navigate to list page
      if (!pkg) {
        this.router.navigate(['./portal/desktop'])
      } else {
        this.onActive(
          pkg,
          !this.navigation.hasSubtab(this.getLink(pkg.manifest.id)),
        )
      }
    }),
  )

  getLink(id: string): string {
    return toRouterLink(id)
  }

  onActive({ icon, manifest }: PackageDataEntry, active: boolean): void {
    if (!active) return

    this.navigation.addTab({
      icon,
      title: manifest.title,
      routerLink: this.getLink(manifest.id),
    })
  }
}
