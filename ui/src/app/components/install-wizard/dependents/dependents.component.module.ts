import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { DependentsComponent } from './dependents.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    DependentsComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [DependentsComponent],
})
export class DependentsComponentModule { }
