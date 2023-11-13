import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { ResponsiveColDirective } from '@start9labs/shared'

import { SkeletonComponent } from './skeleton.component'

@NgModule({
  imports: [CommonModule, IonicModule, ResponsiveColDirective],
  declarations: [SkeletonComponent],
  exports: [SkeletonComponent],
})
export class SkeletonModule {}
