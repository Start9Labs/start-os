import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { EnumListPage } from './enum-list.page'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [EnumListPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
  ],
  exports: [EnumListPage],
})
export class EnumListPageModule { }