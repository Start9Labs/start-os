import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { OSWelcomePage } from './os-welcome.page'
import { SharingModule } from 'src/app/modules/sharing.module'
import { FormsModule } from '@angular/forms'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    SharingModule,
  ],
  declarations: [OSWelcomePage],
})
export class OSWelcomePageModule { }
