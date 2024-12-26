import { Routes } from '@angular/router'
import { PortalComponent } from './portal.component'

const ROUTES: Routes = [
  {
    path: '',
    component: PortalComponent,
    children: [
      {
        redirectTo: 'services',
        pathMatch: 'full',
        path: '',
      },
      {
        path: 'services',
        loadChildren: () =>
          import('./routes/service/service.module').then(m => m.ServiceModule),
      },
      {
        path: 'system',
        loadChildren: () =>
          import('./routes/system/system.module').then(m => m.SystemModule),
      },
    ],
  },
]

export default ROUTES
