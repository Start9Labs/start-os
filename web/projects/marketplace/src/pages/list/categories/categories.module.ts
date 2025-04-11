import { TuiIcon, TuiAppearance } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { TuiSkeleton } from '@taiga-ui/kit'

import { CategoriesComponent } from './categories.component'
import { RouterModule } from '@angular/router'

@NgModule({
  imports: [RouterModule, CommonModule, TuiAppearance, TuiIcon, TuiSkeleton],
  declarations: [CategoriesComponent],
  exports: [CategoriesComponent],
})
export class CategoriesModule {}
