import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'

const ROUTES: Routes = [
  {
    path: 'snek',
    loadComponent: () =>
      import('./snek/snek.component').then(m => m.SnekComponent),
  },
]

@NgModule({
  imports: [RouterModule.forChild(ROUTES)],
  declarations: [],
  exports: [],
})
export class SystemModule {}
