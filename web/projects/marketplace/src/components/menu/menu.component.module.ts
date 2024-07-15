import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { SharedPipesModule } from '@start9labs/shared'

import { MenuComponent } from './menu.component'
import { TuiLoader, TuiIcon, TuiButton, TuiAppearance } from '@taiga-ui/core'
import { TuiActiveZone, TuiLet } from '@taiga-ui/cdk'
import { TuiSidebar } from '@taiga-ui/addon-mobile'
import { SearchModule } from '../../pages/list/search/search.module'
import { CategoriesModule } from '../../pages/list/categories/categories.module'
import { StoreIconComponentModule } from '../store-icon/store-icon.component.module'

@NgModule({
  imports: [
    CommonModule,
    SharedPipesModule,
    SearchModule,
    CategoriesModule,
    TuiActiveZone,
    ...TuiSidebar,
    TuiLoader,
    TuiButton,
    CategoriesModule,
    StoreIconComponentModule,
    TuiLet,
    TuiAppearance,
    TuiIcon,
  ],
  declarations: [MenuComponent],
  exports: [MenuComponent],
})
export class MenuModule {}
