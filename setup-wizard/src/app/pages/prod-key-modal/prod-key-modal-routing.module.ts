import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { ProdKeyModal } from './prod-key-modal.page'

const routes: Routes = [
  {
    path: '',
    component: ProdKeyModal,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ProdKeyModalRoutingModule { }
