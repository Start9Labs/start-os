import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'

import { CategoriesComponent } from './categories.component'
import { TuiAppearanceModule, TuiIconModule } from '@taiga-ui/experimental'
import { RouterModule } from '@angular/router'

@NgModule({
  imports: [RouterModule, CommonModule, TuiAppearanceModule, TuiIconModule],
  declarations: [CategoriesComponent],
  exports: [CategoriesComponent],
})
export class CategoriesModule {}
