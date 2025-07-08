import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiIcon } from '@taiga-ui/core'
import { SearchComponent } from './search.component'

@NgModule({
  imports: [FormsModule, CommonModule, TuiIcon],
  declarations: [SearchComponent],
  exports: [SearchComponent],
})
export class SearchModule {}
