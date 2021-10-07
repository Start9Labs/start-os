import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { RecoverPage } from './recover.page'

const routes: Routes = [
  {
    path: '',
    component: RecoverPage,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class RecoverPageRoutingModule { }
