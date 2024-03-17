import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, Router, RouterOutlet } from '@angular/router'
import { PatchDB } from 'patch-db-client'
import { distinctUntilChanged, filter, map, switchMap, tap } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <ng-container *ngIf="service$ | async" />
    <router-outlet />
  `,
  host: { class: 'g-page' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, RouterOutlet],
})
export class ServiceOutletComponent {
  private readonly patch = inject(PatchDB<DataModel>)
  private readonly route = inject(ActivatedRoute)
  private readonly router = inject(Router)

  readonly service$ = this.router.events.pipe(
    map(() => this.route.firstChild?.snapshot.paramMap?.get('pkgId')),
    filter(Boolean),
    distinctUntilChanged(),
    switchMap(id => this.patch.watch$('package-data', id)),
    tap(pkg => {
      // if package disappears, navigate to list page
      if (!pkg) {
        this.router.navigate(['./portal/dashboard'])
      }
    }),
  )
}
