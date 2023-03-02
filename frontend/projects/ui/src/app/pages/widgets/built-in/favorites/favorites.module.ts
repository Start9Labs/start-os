import { NgModule } from '@angular/core'
import { FavoritesComponent } from './favorites.component'
import { IonicModule } from '@ionic/angular'

@NgModule({
  imports: [IonicModule],
  declarations: [FavoritesComponent],
  exports: [FavoritesComponent],
})
export class FavoritesModule {}
