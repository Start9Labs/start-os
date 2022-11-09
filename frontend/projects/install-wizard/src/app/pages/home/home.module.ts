import { NgModule } from '@angular/core'
import { RouterModule, Routes } from '@angular/router'
import { CommonModule } from '@angular/common'
import { IonicModule } from '@ionic/angular'
import { FormsModule } from '@angular/forms'
import { HomePage } from './home.page'
import { SwiperModule } from 'swiper/angular'
import { UnitConversionPipesModule } from '@start9labs/shared'

const routes: Routes = [
  {
    path: '',
    component: HomePage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    IonicModule,
    RouterModule.forChild(routes),
    SwiperModule,
    UnitConversionPipesModule,
  ],
  declarations: [HomePage],
})
export class HomePageModule {}
