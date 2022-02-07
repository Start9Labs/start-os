import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { AppReleaseNotes } from './app-release-notes.page'
import { MarketplacePipesModule } from '../pipes/marketplace-pipes.module'

const routes: Routes = [
  {
    path: '',
    component: AppReleaseNotes,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    SharedPipesModule,
    MarketplacePipesModule,
  ],
  declarations: [AppReleaseNotes],
})
export class ReleaseNotesModule {}
