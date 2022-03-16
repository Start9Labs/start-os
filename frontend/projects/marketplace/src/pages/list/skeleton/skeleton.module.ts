import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'

import { SkeletonComponent } from './skeleton.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [SkeletonComponent],
  exports: [SkeletonComponent],
})
export class SkeletonModule {}
