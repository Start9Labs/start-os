import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { BadgeMenuComponent } from './badge-menu.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    BadgeMenuComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [BadgeMenuComponent],
})
export class BadgeMenuComponentModule { }
