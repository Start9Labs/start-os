import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { ExperimentalFeaturesPage } from './experimental-features.page'
import { EmverPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: ExperimentalFeaturesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    EmverPipesModule,
  ],
  declarations: [ExperimentalFeaturesPage],
})
export class ExperimentalFeaturesPageModule {}
