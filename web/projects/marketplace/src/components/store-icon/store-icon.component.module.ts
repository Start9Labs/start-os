import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiIconModule } from '@taiga-ui/experimental'
import { StoreIconComponent } from './store-icon.component'

@NgModule({
  declarations: [StoreIconComponent],
  imports: [CommonModule, TuiIconModule],
  exports: [StoreIconComponent],
})
export class StoreIconComponentModule {}
