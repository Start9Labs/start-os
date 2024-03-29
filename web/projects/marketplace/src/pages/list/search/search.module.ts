import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { ResponsiveColModule } from '@start9labs/shared'

import { SearchComponent } from './search.component'

@NgModule({
  imports: [IonicModule, FormsModule, CommonModule, ResponsiveColModule],
  declarations: [SearchComponent],
  exports: [SearchComponent],
})
export class SearchModule {}
