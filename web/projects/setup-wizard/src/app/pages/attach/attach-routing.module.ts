import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { AttachPage } from './attach.page'

const routes: Routes = [
  {
    path: '',
    component: AttachPage,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AttachPageRoutingModule {}
