import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { EmverPipesModule } from '@start9labs/shared'
import { TuiCheckboxLabeledModule, TuiPromptModule } from '@taiga-ui/kit'
import { ExperimentalFeaturesPage } from './experimental-features.page'
import { FormsModule } from '@angular/forms'

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
    TuiPromptModule,
    RouterModule.forChild(routes),
    EmverPipesModule,
    TuiCheckboxLabeledModule,
    FormsModule,
  ],
  declarations: [ExperimentalFeaturesPage],
})
export class ExperimentalFeaturesPageModule {}
