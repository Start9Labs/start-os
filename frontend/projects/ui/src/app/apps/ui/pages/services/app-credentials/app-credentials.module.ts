import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppCredentialsPage } from './app-credentials.page'
import {
  SharedPipesModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { SkeletonListComponentModule } from 'src/app/common/skeleton-list/skeleton-list.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppCredentialsPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    TextSpinnerComponentModule,
    SkeletonListComponentModule,
  ],
  declarations: [AppCredentialsPage],
})
export class AppCredentialsPageModule {}
