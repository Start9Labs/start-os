import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { DependentsComponent } from './dependents.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'

@NgModule({
  declarations: [DependentsComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharedPipesModule,
  ],
  exports: [DependentsComponent],
})
export class DependentsComponentModule {}
