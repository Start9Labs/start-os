import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { IonicModule } from '@ionic/angular';
import { FormsModule } from '@angular/forms';
import { EmbassyPage } from './embassy.page';
import { PasswordPageModule } from '../password/password.module';

import { EmbassyPageRoutingModule } from './embassy-routing.module';


@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    EmbassyPageRoutingModule,
    PasswordPageModule,
  ],
  declarations: [EmbassyPage]
})
export class EmbassyPageModule {}
