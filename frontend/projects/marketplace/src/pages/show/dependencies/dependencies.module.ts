import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { SharedPipesModule } from '@start9labs/shared'

import { DependenciesComponent } from './dependencies.component'

@NgModule({
  imports: [CommonModule, RouterModule, IonicModule, SharedPipesModule],
  declarations: [DependenciesComponent],
  exports: [DependenciesComponent],
})
export class DependenciesModule {}
