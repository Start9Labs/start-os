import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { RecoverPage } from './recover.page'
import { PasswordPageModule } from '../password/password.module'
import { ProdKeyModalModule } from '../prod-key-modal/prod-key-modal.module'
import { RecoverPageRoutingModule } from './recover-routing.module'
import { PipesModule } from 'src/app/pipes/pipe.module'


@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RecoverPageRoutingModule,
    PasswordPageModule,
    ProdKeyModalModule,
    PipesModule,
  ],
  declarations: [RecoverPage],
})
export class RecoverPageModule { }
