import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import { FilterUpdatesPipe, UpdatesPage } from './updates.page'
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module'
import {
  EmverDisplayPipe,
  EmverPipesModule,
  MarkdownPipeModule,
  SharedPipesModule,
} from '@start9labs/shared'
import { SkeletonListComponentModule } from 'src/app/components/skeleton-list/skeleton-list.component.module'
import { RoundProgressModule } from 'angular-svg-round-progressbar'
import { InstallProgressPipeModule } from 'src/app/pipes/install-progress/install-progress.module'
import { StoreIconComponentModule } from 'src/app/components/store-icon/store-icon.component.module'
import { MimeTypePipeModule } from '@start9labs/marketplace'

const routes: Routes = [
  {
    path: '',
    component: UpdatesPage,
  },
]

@NgModule({
  declarations: [UpdatesPage, FilterUpdatesPipe],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    SharedPipesModule,
    SkeletonListComponentModule,
    MarkdownPipeModule,
    RoundProgressModule,
    InstallProgressPipeModule,
    StoreIconComponentModule,
    EmverPipesModule,
    MimeTypePipeModule,
  ],
})
export class UpdatesPageModule {}
