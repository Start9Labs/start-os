import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule, Routes } from '@angular/router'
import {
  MimeTypePipeModule,
  StoreIconComponentModule,
} from '@start9labs/marketplace'
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SharedPipesModule,
} from '@start9labs/shared'
import { RoundProgressModule } from 'angular-svg-round-progressbar'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { SkeletonListComponentModule } from 'src/app/common/skeleton-list/skeleton-list.component.module'
import { UpdatesPage } from './updates.page'
import { InstallProgressPipe } from './install-progress.pipe'
import { FilterUpdatesPipe } from './filter-updates.pipe'

const routes: Routes = [
  {
    path: '',
    component: UpdatesPage,
  },
]

@NgModule({
  declarations: [UpdatesPage, FilterUpdatesPipe, InstallProgressPipe],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
    SharedPipesModule,
    SkeletonListComponentModule,
    MarkdownPipeModule,
    RoundProgressModule,
    StoreIconComponentModule,
    EmverPipesModule,
    MimeTypePipeModule,
  ],
})
export class UpdatesPageModule {}
