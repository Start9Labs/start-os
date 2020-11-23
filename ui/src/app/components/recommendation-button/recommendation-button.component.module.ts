import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { RecommendationButtonComponent } from './recommendation-button.component'
import { IonicModule } from '@ionic/angular'
import { RouterModule } from '@angular/router'
import { SharingModule } from 'src/app/modules/sharing.module'

@NgModule({
  declarations: [
    RecommendationButtonComponent,
  ],
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild([]),
    SharingModule,
  ],
  exports: [RecommendationButtonComponent],
})
export class RecommendationButtonComponentModule { }
