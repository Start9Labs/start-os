import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { GenericInputComponent } from './generic-input.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [GenericInputComponent],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    RouterModule.forChild([]),
    SharedPipesModule,
  ],
  exports: [GenericInputComponent],
})
export class GenericInputComponentModule {}
