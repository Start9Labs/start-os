import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { EmbassyPage } from './embassy.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { EmbassyPageRoutingModule } from './embassy-routing.module'
import { PipesModule } from 'src/app/pipes/pipe.module'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    EmbassyPageRoutingModule,
    PasswordPageModule,
    PipesModule,
  ],
  declarations: [EmbassyPage],
})
export class EmbassyPageModule { }
