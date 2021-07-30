import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { IonicModule } from '@ionic/angular'
import { AppConfigPage } from './app-config.page'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
import { AppConfigListPageModule } from 'src/app/modals/app-config-list/app-config-list.module'
import { AppConfigObjectPageModule } from 'src/app/modals/app-config-object/app-config-object.module'
import { AppConfigUnionPageModule } from 'src/app/modals/app-config-union/app-config-union.module'
import { AppConfigValuePageModule } from 'src/app/modals/app-config-value/app-config-value.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { TextSpinnerComponentModule } from 'src/app/components/text-spinner/text-spinner.component.module'
import { SubNavComponentModule } from 'src/app/components/sub-nav/sub-nav.component.module'

@NgModule({
  declarations: [AppConfigPage],
  imports: [
    ObjectConfigComponentModule,
    AppConfigListPageModule,
    AppConfigObjectPageModule,
    AppConfigUnionPageModule,
    AppConfigValuePageModule,
    TextSpinnerComponentModule,
    SharingModule,
    CommonModule,
    FormsModule,
    IonicModule,
    SubNavComponentModule,
  ],
  entryComponents: [AppConfigPage],
  exports: [AppConfigPage],
})
export class AppConfigPageModule { }
