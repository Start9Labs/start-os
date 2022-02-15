import { NgModule } from '@angular/core'
import { CommonModule } from '@angular/common'
import { Routes, RouterModule } from '@angular/router'
import { IonicModule } from '@ionic/angular'
import { MarketplaceShowPage } from './marketplace-show.page'
import {
  SharedPipesModule,
  EmverPipesModule,
  MarkdownPipeModule,
  TextSpinnerComponentModule,
} from '@start9labs/shared'
import { InstallWizardComponentModule } from 'src/app/components/install-wizard/install-wizard.component.module'
import { MarketplacePipesModule } from '../pipes/marketplace-pipes.module'

const routes: Routes = [
  {
    path: '',
    component: MarketplaceShowPage,
  },
]

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    RouterModule.forChild(routes),
    TextSpinnerComponentModule,
    SharedPipesModule,
    EmverPipesModule,
    MarkdownPipeModule,
    MarketplacePipesModule,
    InstallWizardComponentModule,
  ],
  declarations: [MarketplaceShowPage],
})
export class MarketplaceShowPageModule {}
