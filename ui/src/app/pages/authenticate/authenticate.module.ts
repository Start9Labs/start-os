import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { IonicModule } from '@ionic/angular';
import { AuthenticatePageRoutingModule } from './authenticate-routing.module';
import { AuthenticatePage } from './authenticate.page';
import { PwaBackComponentModule } from 'src/app/components/pwa-back-button/pwa-back.component.module';
import { BadgeMenuComponentModule } from 'src/app/components/badge-menu-button/badge-menu.component.module';

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    AuthenticatePageRoutingModule,
    PwaBackComponentModule,
    BadgeMenuComponentModule,
  ],
  declarations: [AuthenticatePage],
})
export class AuthenticatePageModule { }
