import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { SharedPipesModule } from '@start9labs/shared'
import { TuiLet } from '@taiga-ui/cdk'
import {
  TuiAppearance,
  TuiButton,
  TuiIcon,
  TuiLoader,
  TuiPopup,
} from '@taiga-ui/core'
import { TuiDrawer, TuiSkeleton } from '@taiga-ui/kit'
import { CategoriesModule } from '../../pages/list/categories/categories.module'
import { SearchModule } from '../../pages/list/search/search.module'
import { StoreIconComponentModule } from '../store-icon/store-icon.component.module'
import { MenuComponent } from './menu.component'

@NgModule({
  imports: [
    CommonModule,
    SharedPipesModule,
    SearchModule,
    CategoriesModule,
    TuiLoader,
    TuiButton,
    CategoriesModule,
    StoreIconComponentModule,
    TuiLet,
    TuiAppearance,
    TuiIcon,
    TuiSkeleton,
    TuiDrawer,
    TuiPopup,
  ],
  declarations: [MenuComponent],
  exports: [MenuComponent],
})
export class MenuModule {}
