import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { SuccessPage } from './success.page'

const routes: Routes = [
  {
    path: '',
    component: SuccessPage,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class SuccessPageRoutingModule { }
