import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { AppConfigPage } from './app-config.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { SubNavModule } from 'src/app/modules/subnav.module'

@NgModule({
  declarations: [AppConfigPage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    SharingModule,
    SubNavModule,
  ],
  entryComponents: [AppConfigPage],
  exports: [AppConfigPage],
})
export class AppConfigPageModule { }
