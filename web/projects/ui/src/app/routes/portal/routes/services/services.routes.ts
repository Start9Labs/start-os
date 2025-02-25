import { inject } from '@angular/core'
import { ActivatedRouteSnapshot, ResolveFn, Routes } from '@angular/router'
import { defer, map, Observable, of } from 'rxjs'
import { share } from 'rxjs/operators'
import { ApiService } from 'src/app/services/api/embassy-api.service'

import { ServiceOutletComponent } from './routes/outlet.component'
import { ServiceRoute } from './routes/service.component'

export const ROUTES: Routes = [
  {
    path: ':pkgId',
    component: ServiceOutletComponent,
    children: [
      {
        path: '',
        component: ServiceRoute,
      },
      {
        path: 'actions',
        loadComponent: () => import('./routes/actions.component'),
      },
      {
        path: 'instructions',
        loadComponent: () => import('./routes/markdown.component'),
        resolve: { content: getStatic('instructions.md') },
        canActivate: [
          ({ paramMap }: ActivatedRouteSnapshot) => {
            inject(ApiService)
              .setDbValue(['ack-instructions', paramMap.get('pkgId')!], true)
              .catch(e => console.error('Failed to mark as seen', e))

            return true
          },
        ],
      },
      {
        path: 'interface/:interfaceId',
        loadComponent: () => import('./routes/interface.component'),
      },
      {
        path: 'logs',
        loadComponent: () => import('./routes/logs.component'),
      },
      {
        path: 'about',
        loadComponent: () => import('./routes/about.component'),
        resolve: { content: getStatic('LICENSE.md') },
      },
    ],
  },
  {
    path: '',
    pathMatch: 'full',
    loadComponent: () => import('./dashboard/dashboard.component'),
  },
]

function getStatic(
  path: 'LICENSE.md' | 'instructions.md',
): ResolveFn<Observable<string>> {
  return ({ paramMap }: ActivatedRouteSnapshot) =>
    of(inject(ApiService)).pipe(
      map(api =>
        defer(() => api.getStaticInstalled(paramMap.get('pkgId')!, path)).pipe(
          share(),
        ),
      ),
    )
}

export default ROUTES
