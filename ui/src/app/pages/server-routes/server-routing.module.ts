import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'
import { AuthGuard } from '../../guards/auth.guard'

const routes: Routes = [
  {
    path: '',
    canActivate: [AuthGuard],
    loadChildren: () => import('./server-show/server-show.module').then(m => m.ServerShowPageModule),
  },
  {
    path: 'specs',
    canActivate: [AuthGuard],
    loadChildren: () => import('./server-specs/server-specs.module').then(m => m.ServerSpecsPageModule),
  },
  {
    path: 'metrics',
    canActivate: [AuthGuard],
    loadChildren: () => import('./server-metrics/server-metrics.module').then(m => m.ServerMetricsPageModule),
  },
  {
    path: 'config',
    canActivate: [AuthGuard],
    loadChildren: () => import('./server-config/server-config.module').then(m => m.ServerConfigPageModule),
  },
  {
    path: 'wifi',
    canActivate: [AuthGuard],
    loadChildren: () => import('./wifi/wifi.module').then(m => m.WifiListPageModule),
  },
  {
    path: 'lan',
    canActivate: [AuthGuard],
    loadChildren: () => import('./lan/lan.module').then(m => m.LANPageModule),
  },
  {
    path: 'developer',
    canActivate: [AuthGuard],
    loadChildren: () => import('./developer-routes/developer-routing.module').then( m => m.DeveloperRoutingModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ServerRoutingModule { }