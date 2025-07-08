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
    loadComponent: () => import('./logs.component'),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
})
export default class DiagnosticModule {}
