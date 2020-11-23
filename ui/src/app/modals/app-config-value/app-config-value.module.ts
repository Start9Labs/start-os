import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { AppConfigValuePage } from './app-config-value.page'
import { ConfigHeaderComponentModule } from 'src/app/components/config-header/config-header.component.module'

@NgModule({
  declarations: [AppConfigValuePage],
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    ConfigHeaderComponentModule,
  ],
  entryComponents: [AppConfigValuePage],
  exports: [AppConfigValuePage],
})
export class AppConfigValuePageModule { }
