import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { ServiceUiMenuComponent } from './service-ui-menu.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    ServiceUiMenuComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [ServiceUiMenuComponent],
})
export class ServiceUiMenuComponentModule { }
