import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { AppConfigUnionPage } from './app-config-union.page'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
import { FormsModule } from '@angular/forms'
import { ConfigHeaderComponentModule } from 'src/app/components/config-header/config-header.component.module'


@NgModule({
  declarations: [AppConfigUnionPage],
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    ObjectConfigComponentModule,
    ConfigHeaderComponentModule,
  ],
  entryComponents: [AppConfigUnionPage],
  exports: [AppConfigUnionPage],
})
export class AppConfigUnionPageModule { }