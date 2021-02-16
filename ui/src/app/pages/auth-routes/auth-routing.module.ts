import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'pin',
    pathMatch: 'full',
  },
  {
    path: 'pin',
    loadChildren: () => import('./auth-pin/auth-pin.module').then(m => m.AuthPinPageModule),
  },
  {
    path: 'password',
    loadChildren: () => import('./auth-password/auth-password.module').then(m => m.AuthPasswordPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AuthRoutingModule { }