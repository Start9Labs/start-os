import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { ObjectConfigComponent, ObjectConfigItemComponent } from './object-config.component'
import { IonicModule } from '@ionic/angular'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    ObjectConfigComponent,
    ObjectConfigItemComponent,
  ],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    SharingModule,
  ],
  exports: [
    ObjectConfigComponent,
    ObjectConfigItemComponent,
  ],
})
export class ObjectConfigComponentModule { }
