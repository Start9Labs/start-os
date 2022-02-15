import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { PreferencesPage } from './preferences.page'
import { Routes, RouterModule } from '@angular/router'
import { SharedPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: PreferencesPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
  ],
  declarations: [PreferencesPage],
})
export class PreferencesPageModule {}
