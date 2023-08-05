import { NgModule } from "@angular/core";
import { CommonModule } from "@angular/common";
import { FormsModule } from "@angular/forms";
import { RouterModule, Routes } from "@angular/router";
import { IonicModule } from "@ionic/angular";
import {
  EmverPipesModule,
  ResponsiveColDirective,
  SharedPipesModule,
} from "@start9labs/shared";
import {
  CategoriesModule,
  FilterPackagesPipeModule,
  ItemModule,
  MarketplaceHeaderModule,
  SearchModule,
  SkeletonModule,
  StoreIconComponentModule,
<<<<<<< HEAD
} from "@start9labs/marketplace";
import { BadgeMenuComponentModule } from "src/app/common/badge-menu-button/badge-menu.component.module";
import { MarketplaceStatusModule } from "../marketplace-status/marketplace-status.module";
import { MarketplaceListPage } from "./marketplace-list.page";
import { MarketplaceSettingsPageModule } from "./marketplace-settings/marketplace-settings.module";
=======
} from '@start9labs/marketplace'
import { BadgeMenuComponentModule } from 'src/app/common/badge-menu-button/badge-menu.component.module'
import { MarketplaceStatusModule } from '../marketplace-status/marketplace-status.module'
import { MarketplaceListPage } from './marketplace-list.page'
import { MarketplaceSettingsPageModule } from './marketplace-settings/marketplace-settings.module'
import { TuiNotificationModule } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
>>>>>>> 4a3fad0dd (styling cleanup)

const routes: Routes = [
  {
    path: "",
    component: MarketplaceListPage,
  },
];

@NgModule({
  imports: [
    CommonModule,
    IonicModule,
    FormsModule,
    RouterModule.forChild(routes),
    SharedPipesModule,
    EmverPipesModule,
    FilterPackagesPipeModule,
    MarketplaceStatusModule,
    BadgeMenuComponentModule,
    ItemModule,
    CategoriesModule,
    SearchModule,
    SkeletonModule,
    MarketplaceSettingsPageModule,
    StoreIconComponentModule,
    ResponsiveColDirective,
    MarketplaceHeaderModule,
    TuiNotificationModule,
    NgDompurifyModule,
  ],
  declarations: [MarketplaceListPage],
  exports: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
