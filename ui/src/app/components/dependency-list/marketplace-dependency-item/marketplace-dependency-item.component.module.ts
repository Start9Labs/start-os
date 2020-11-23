import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { MarketplaceDependencyItemComponent } from './marketplace-dependency-item.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { InformationPopoverComponentModule } from '../../information-popover/information-popover.component.module'
import { StatusComponentModule } from '../../status/status.component.module'

@NgModule({
  declarations: [MarketplaceDependencyItemComponent],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    InformationPopoverComponentModule,
    StatusComponentModule,
  ],
  exports: [MarketplaceDependencyItemComponent],
})
export class MarketplaceDependencyItemComponentModule { }
