import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { DependenciesComponent } from './dependencies.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { StatusComponentModule } from '../../status/status.component.module'

@NgModule({
  declarations: [
    DependenciesComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    StatusComponentModule,
  ],
  exports: [DependenciesComponent],
})
export class DependenciesComponentModule { }
