import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { TuiProgressModule } from '@taiga-ui/kit'
import { LoadingPage } from './loading.page'
import { LoadingPageRoutingModule } from './loading-routing.module'
import { IonicModule } from '@ionic/angular'

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    TuiProgressModule,
    LoadingPageRoutingModule,
  ],
  declarations: [LoadingPage],
})
export class LoadingPageModule {}
