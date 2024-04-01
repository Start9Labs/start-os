import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { SearchComponent } from './search.component'
import { TuiSvgModule } from '@taiga-ui/core'

@NgModule({
  imports: [FormsModule, CommonModule, TuiSvgModule],
  declarations: [SearchComponent],
  exports: [SearchComponent],
})
export class SearchModule {}
