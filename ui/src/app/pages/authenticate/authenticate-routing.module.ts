import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { AuthenticatePage } from './authenticate.page';

const routes: Routes = [
  {
    path: '',
    component: AuthenticatePage
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AuthenticatePageRoutingModule {}
