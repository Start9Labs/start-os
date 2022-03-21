import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { IonicModule } from '@ionic/angular'

import { CategoriesComponent } from './categories.component'

@NgModule({
  imports: [CommonModule, IonicModule],
  declarations: [CategoriesComponent],
  exports: [CategoriesComponent],
})
export class CategoriesModule {}
