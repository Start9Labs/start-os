import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'

import { CategoriesComponent } from './categories.component'
import { TuiSvgModule } from '@taiga-ui/core'

@NgModule({
  imports: [CommonModule, TuiSvgModule],
  declarations: [CategoriesComponent],
  exports: [CategoriesComponent],
})
export class CategoriesModule {}
