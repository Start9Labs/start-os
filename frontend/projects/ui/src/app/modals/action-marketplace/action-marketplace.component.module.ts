import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { FormsModule } from '@angular/forms'
import { ActionMarketplaceComponent } from './action-marketplace.component'

@NgModule({
  declarations: [ActionMarketplaceComponent],
  imports: [CommonModule, IonicModule, FormsModule, RouterModule.forChild([])],
  exports: [ActionMarketplaceComponent],
})
export class ActionMarketplaceComponentModule {}
