import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { ItemComponent } from './item.component'
import { MimeTypePipeModule } from '../../../pipes/mime-type.pipe'

@NgModule({
  declarations: [ItemComponent],
  exports: [ItemComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule,
    SharedPipesModule,
    MimeTypePipeModule,
  ],
})
export class ItemModule {}
