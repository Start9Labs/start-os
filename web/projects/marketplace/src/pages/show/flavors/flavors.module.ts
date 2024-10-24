import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ResponsiveColModule, SharedPipesModule } from '@start9labs/shared'
import { FlavorsComponent } from './flavors.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    SharedPipesModule,
    ResponsiveColModule,
  ],
  declarations: [FlavorsComponent],
  exports: [FlavorsComponent],
})
export class FlavorsModule {}
