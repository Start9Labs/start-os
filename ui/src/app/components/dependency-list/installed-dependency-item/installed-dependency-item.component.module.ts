import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { InstalledDependencyItemComponent } from './installed-dependency-item.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { InformationPopoverComponentModule } from '../../information-popover/information-popover.component.module'
import { StatusComponentModule } from '../../status/status.component.module'

@NgModule({
  declarations: [InstalledDependencyItemComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    InformationPopoverComponentModule,
    StatusComponentModule,
  ],
  exports: [InstalledDependencyItemComponent],
})
export class InstalledDependencyItemComponentModule { }
