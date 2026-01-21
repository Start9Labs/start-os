import { TuiIcon, TuiAppearance } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { TuiSkeleton } from '@taiga-ui/kit'
import { LocalizePipe } from '@start9labs/shared'

import { CategoriesComponent } from './categories.component'
import { RouterModule } from '@angular/router'

@NgModule({
  imports: [RouterModule, CommonModule, TuiAppearance, TuiIcon, TuiSkeleton, LocalizePipe],
  declarations: [CategoriesComponent],
  exports: [CategoriesComponent],
})
export class CategoriesModule {}
