import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppConfigObjectPage } from './app-config-object.page'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
import { ConfigHeaderComponentModule } from 'src/app/components/config-header/config-header.component.module'

@NgModule({
  declarations: [AppConfigObjectPage],
  imports: [
    CommonModule,
    IonicModule,
    ObjectConfigComponentModule,
    ConfigHeaderComponentModule,
  ],
  entryComponents: [AppConfigObjectPage],
  exports: [AppConfigObjectPage],
})
export class AppConfigObjectPageModule { }