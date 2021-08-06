import { NgModule } from '@angular/core'
import { AppConfigListPageModule } from 'src/app/modals/app-config-list/app-config-list.module'
import { AppConfigObjectPageModule } from 'src/app/modals/app-config-object/app-config-object.module'
import { AppConfigUnionPageModule } from 'src/app/modals/app-config-union/app-config-union.module'
import { AppConfigValuePageModule } from 'src/app/modals/app-config-value/app-config-value.module'
import { SubNavComponentModule } from '../components/sub-nav/sub-nav.component.module'

@NgModule({
    imports: [
      AppConfigListPageModule,
      AppConfigObjectPageModule,
      AppConfigUnionPageModule,
      AppConfigValuePageModule,
      SubNavComponentModule,
    ],
    exports: [
      AppConfigListPageModule,
      AppConfigObjectPageModule,
      AppConfigUnionPageModule,
      AppConfigValuePageModule,
      SubNavComponentModule,
    ],
})
export class SubNavModule { }