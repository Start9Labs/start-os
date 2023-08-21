import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { SharedPipesModule } from '@start9labs/shared'

import { SidebarComponent } from './sidebar.component'
import { TuiButtonModule, TuiLoaderModule } from '@taiga-ui/core'
import { TuiActiveZoneModule, TuiLetModule } from '@taiga-ui/cdk'
import { TuiSidebarModule } from '@taiga-ui/addon-mobile'
import { SearchModule } from '../../pages/list/search/search.module'
import { CategoriesModule } from '../../pages/list/categories/categories.module'
import { StoreIconComponentModule } from '../store-icon/store-icon.component.module'

@NgModule({
  imports: [
    CommonModule,
    SharedPipesModule,
    SearchModule,
    CategoriesModule,
    TuiActiveZoneModule,
    TuiSidebarModule,
    TuiLoaderModule,
    TuiButtonModule,
    CategoriesModule,
    StoreIconComponentModule,
    TuiLetModule,
  ],
  declarations: [SidebarComponent],
  exports: [SidebarComponent],
})
export class SidebarModule {}
