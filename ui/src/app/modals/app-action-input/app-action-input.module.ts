import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppActionInputPage } from './app-action-input.page'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
import { ConfigHeaderComponentModule } from 'src/app/components/config-header/config-header.component.module'

@NgModule({
  declarations: [AppActionInputPage],
  imports: [
    CommonModule,
    IonicModule,
    ObjectConfigComponentModule,
    ConfigHeaderComponentModule,
  ],
  entryComponents: [AppActionInputPage],
  exports: [AppActionInputPage],
})
export class AppActionInputPageModule { }