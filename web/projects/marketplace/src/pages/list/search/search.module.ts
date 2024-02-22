import { CommonModule } from '@angular/common'
import { NgModule } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { SearchComponent } from './search.component'

@NgModule({
  imports: [FormsModule, CommonModule],
  declarations: [SearchComponent],
  exports: [SearchComponent],
})
export class SearchModule {}
