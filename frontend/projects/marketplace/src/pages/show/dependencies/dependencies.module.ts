import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule, SharedPipesModule } from '@start9labs/shared'

import { DependenciesComponent } from './dependencies.component'

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    SharedPipesModule,
    EmverPipesModule,
  ],
  declarations: [DependenciesComponent],
  exports: [DependenciesComponent],
})
export class DependenciesModule {}
