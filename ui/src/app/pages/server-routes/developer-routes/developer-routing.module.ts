import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () => import('./dev-options/dev-options.module').then(m => m.DevOptionsPageModule),
  },
  {
    path: 'ssh-keys',
    loadChildren: () => import('./dev-ssh-keys/dev-ssh-keys.module').then(m => m.DevSSHKeysPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class DeveloperRoutingModule { }