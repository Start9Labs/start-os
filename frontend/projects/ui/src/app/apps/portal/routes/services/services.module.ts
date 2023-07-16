import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { ServicesComponent } from './services.component'

const ROUTES: Routes = [
  {
    path: '',
    component: ServicesComponent,
  },
  {
    path: ':pkgId',
    loadChildren: () =>
      import('./service/service.module').then(m => m.ServiceModule),
  },
]

@NgModule({
  imports: [CommonModule, RouterModule.forChild(ROUTES)],
  declarations: [ServicesComponent],
  exports: [ServicesComponent],
})
export class ServicesModule {}
