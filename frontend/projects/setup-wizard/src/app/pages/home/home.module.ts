import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { HomePage } from './home.page'
import { PasswordPageModule } from '../../modals/password/password.module'
import { HomePageRoutingModule } from './home-routing.module'
import { SwiperModule } from 'swiper/angular'

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    HomePageRoutingModule,
    PasswordPageModule,
    SwiperModule,
  ],
  declarations: [HomePage],
})
export class HomePageModule {}
