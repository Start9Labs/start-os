import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./developer-list/developer-list.module').then(
        m => m.DeveloperPageModule,
      ),
  },
  {
    path: 'config',
    loadChildren: () =>
      import('./config/config.module').then(m => m.ConfigPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class DeveloperRoutingModule {}
