import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { SkeletonListComponent } from './skeleton-list.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'

@NgModule({
  declarations: [
    SkeletonListComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
  ],
  exports: [SkeletonListComponent],
})
export class SkeletonListComponentModule { }
