import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { ServiceComponent } from './service.component'

const ROUTES: Routes = [
  {
    path: '',
    component: ServiceComponent,
  },
]

@NgModule({
  imports: [CommonModule, RouterModule.forChild(ROUTES)],
  declarations: [ServiceComponent],
  exports: [ServiceComponent],
})
export class ServiceModule {}
