import { NgModule } from '@angular/core'
import { Routes, RouterModule } from '@angular/router'

const routes: Routes = [
  {
    path: '',
    redirectTo: 'projects',
    pathMatch: 'full',
  },
  {
    path: 'projects',
    loadChildren: () =>
      import('./developer-list/developer-list.module').then(
        m => m.DeveloperListPageModule,
      ),
  },
  {
    path: 'projects/:projectId',
    loadChildren: () =>
      import('./developer-menu/developer-menu.module').then(
        m => m.DeveloperMenuPageModule,
      ),
  },
  {
    path: 'projects/:projectId/config',
    loadChildren: () =>
      import('./dev-config/dev-config.module').then(m => m.DevConfigPageModule),
  },
  {
    path: 'projects/:projectId/instructions',
    loadChildren: () =>
      import('./dev-instructions/dev-instructions.module').then(
        m => m.DevInstructionsPageModule,
      ),
  },
  {
    path: 'projects/:projectId/manifest',
    loadChildren: () =>
      import('./dev-manifest/dev-manifest.module').then(
        m => m.DevManifestPageModule,
      ),
  },
]

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class DeveloperRoutingModule {}
