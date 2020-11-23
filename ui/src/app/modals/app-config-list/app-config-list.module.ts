import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppConfigListPage } from './app-config-list.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { ConfigHeaderComponentModule } from 'src/app/components/config-header/config-header.component.module'
import { FormsModule } from '@angular/forms'

@NgModule({
  declarations: [AppConfigListPage],
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
    FormsModule,
    ConfigHeaderComponentModule,
  ],
  entryComponents: [AppConfigListPage],
  exports: [AppConfigListPage],
})
export class AppConfigListPageModule { }