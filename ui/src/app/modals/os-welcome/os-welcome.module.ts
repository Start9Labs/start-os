import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { OSWelcomePage } from './os-welcome.page'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    SharingModule,
  ],
  declarations: [OSWelcomePage],
})
export class OSWelcomePageModule { }
