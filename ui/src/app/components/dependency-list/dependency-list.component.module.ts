import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { DependencyListComponent } from './dependency-list.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'
import { InformationPopoverComponentModule } from '../information-popover/information-popover.component.module'
import { StatusComponentModule } from '../status/status.component.module'
import { InstalledDependencyItemComponentModule } from './installed-dependency-item/installed-dependency-item.component.module'
import { MarketplaceDependencyItemComponentModule } from './marketplace-dependency-item/marketplace-dependency-item.component.module'

@NgModule({
  declarations: [
    DependencyListComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
    InformationPopoverComponentModule,
    StatusComponentModule,
    InstalledDependencyItemComponentModule,
    MarketplaceDependencyItemComponentModule,
  ],
  exports: [DependencyListComponent],
})
export class DependencyListComponentModule { }
