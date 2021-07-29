import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () => import('./security-options/security-options.module').then(m => m.SecurityOptionsPageModule),
  },
  {
    path: 'sessions',
    loadChildren: () => import('./sessions/sessions.module').then(m => m.SessionsPageModule),
  },
  {
    path: 'ssh-keys',
    loadChildren: () => import('./ssh-keys/ssh-keys.module').then(m => m.SSHKeysPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SecurityRoutingModule { }