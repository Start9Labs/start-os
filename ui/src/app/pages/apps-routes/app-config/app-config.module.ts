import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { FormsModule } from '@angular/forms'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { AppConfigPage } from './app-config.page'
import { ObjectConfigComponentModule } from 'src/app/components/object-config/object-config.component.module'
import { AppConfigListPageModule } from 'src/app/modals/app-config-list/app-config-list.module'
import { AppConfigObjectPageModule } from 'src/app/modals/app-config-object/app-config-object.module'
import { AppConfigUnionPageModule } from 'src/app/modals/app-config-union/app-config-union.module'
import { AppConfigValuePageModule } from 'src/app/modals/app-config-value/app-config-value.module'
import { SharingModule } from 'src/app/modules/sharing.module'
import { RecommendationButtonComponentModule } from 'src/app/components/recommendation-button/recommendation-button.component.module'
import { InformationPopoverComponentModule } from 'src/app/components/information-popover/information-popover.component.module'

const routes: Routes = [
  {
    path: '',
    component: AppConfigPage,
  },
]

@NgModule({
  imports: [
    ObjectConfigComponentModule,
    AppConfigListPageModule,
    AppConfigObjectPageModule,
    AppConfigUnionPageModule,
    AppConfigValuePageModule,
    SharingModule,
    CommonModule,
    FormsModule,
    IonicModule,
    RouterModule.forChild(routes),
    RecommendationButtonComponentModule,
    InformationPopoverComponentModule,
  ],
  declarations: [AppConfigPage],
})
export class AppConfigPageModule { }
