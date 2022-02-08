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
      import('./dev-config/dev-config.module').then(m => m.DevConfigPageModule),
  },
  {
    path: 'instructions',
    loadChildren: () =>
      import('./dev-instructions/dev-instructions.module').then(
        m => m.DevInstructionsPageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class DeveloperRoutingModule {}
