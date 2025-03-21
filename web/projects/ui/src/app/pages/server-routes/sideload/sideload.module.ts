import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { SideloadPage } from './sideload.page'
import { Routes, RouterModule } from '@angular/router'
import { ExverPipesModule, SharedPipesModule } from '@start9labs/shared'
import { DragNDropDirective } from './dnd.directive'
import { InstallingProgressPipeModule } from 'src/app/pipes/install-progress/install-progress.module'

const routes: Routes = [
  {
    path: '',
    component: SideloadPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    ExverPipesModule,
    InstallingProgressPipeModule,
  ],
  declarations: [SideloadPage, DragNDropDirective],
})
export class SideloadPageModule {}
