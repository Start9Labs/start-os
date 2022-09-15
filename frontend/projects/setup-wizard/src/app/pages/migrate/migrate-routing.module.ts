import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { MigratePage } from './migrate.page'

const routes: Routes = [
  {
    path: '',
    component: MigratePage,
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class MigratePageRoutingModule {}
