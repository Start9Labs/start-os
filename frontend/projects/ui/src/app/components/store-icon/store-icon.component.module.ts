import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { GetIconPipe, StoreIconComponent } from './store-icon.component'

@NgModule({
  declarations: [StoreIconComponent, GetIconPipe],
  imports: [CommonModule, IonicModule],
  exports: [StoreIconComponent],
})
export class StoreIconComponentModule {}
