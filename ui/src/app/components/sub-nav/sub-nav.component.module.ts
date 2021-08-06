import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { SubNavComponent } from './sub-nav.component'
import { IonicModule } from '@ionic/angular'

@NgModule({
  declarations: [
    SubNavComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
  ],
  exports: [SubNavComponent],
})
export class SubNavComponentModule { }
