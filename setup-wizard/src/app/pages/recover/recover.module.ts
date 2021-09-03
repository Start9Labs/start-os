import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { IonicModule } from '@ionic/angular';
import { FormsModule } from '@angular/forms';
import { RecoverPage } from './recover.page';
import { PasswordPageModule } from '../password/password.module';

import { RecoverPageRoutingModule } from './recover-routing.module';


@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RecoverPageRoutingModule,
    PasswordPageModule,
  ],
  declarations: [RecoverPage]
})
export class RecoverPageModule {}
