import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

import { ServiceOutletComponent } from './routes/outlet.component'
import { ServiceRoute } from './routes/service.component'

const ROUTES: Routes = [
  {
    path: '',
    component: ServiceOutletComponent,
    children: [
      {
        path: ':pkgId',
        component: ServiceRoute,
      },
      {
        path: ':pkgId/actions',
        loadComponent: () =>
          import('./routes/actions.component').then(m => m.ServiceActionsRoute),
      },
      {
        path: ':pkgId/interface/:interfaceId',
        loadComponent: () =>
          import('./routes/interface.component').then(
            m => m.ServiceInterfaceRoute,
          ),
      },
      {
        path: ':pkgId/logs',
        loadComponent: () =>
          import('./routes/logs.component').then(m => m.ServiceLogsRoute),
      },
      {
        path: '',
        pathMatch: 'full',
        loadComponent: () =>
          import('./dashboard/dashboard.component').then(
            m => m.DashboardComponent,
          ),
      },
    ],
  },
]

@NgModule({ imports: [RouterModule.forChild(ROUTES)] })
export class ServiceModule {}
