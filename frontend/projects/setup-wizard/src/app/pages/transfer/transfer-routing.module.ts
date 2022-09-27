import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { TransferPage } from './transfer.page'

const routes: Routes = [
  {
    path: '',
    component: TransferPage,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class TransferPageRoutingModule {}
