import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import {
  GuidPipePipesModule,
  UnitConversionPipesModule,
} from '@start9labs/shared'
import { AttachPage } from './attach.page'
import { AttachPageRoutingModule } from './attach-routing.module'

@NgModule({
  declarations: [AttachPage],
  imports: [
    CommonModule,
    IonicModule,
    AttachPageRoutingModule,
    UnitConversionPipesModule,
    GuidPipePipesModule,
  ],
})
export class AttachPageModule {}
