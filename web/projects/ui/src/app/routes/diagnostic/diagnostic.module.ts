import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

const ROUTES: Routes = [
  {
    path: '',
    loadChildren: () =>
      import('./home/home.module').then(m => m.HomePageModule),
  },
  {
    path: 'logs',
    loadChildren: () =>
      import('./logs/logs.module').then(m => m.LogsPageModule),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
})
export class DiagnosticModule {}
