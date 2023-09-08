import { NgModule } from "@angular/core";
import { CommonModule } from "@angular/common";
import { RouterModule, Routes } from "@angular/router";
import { IonicModule } from "@ionic/angular";
import { ResponsiveColDirective, SharedPipesModule } from "@start9labs/shared";
import { FilterPackagesPipeModule, ItemModule } from "@start9labs/marketplace";
import { MarketplaceSidebarModule } from "../components/marketplace-sidebar/marketplace-sidebar.module";
import { MarketplaceListPage } from "./marketplace-list.page";
import { MarketplaceSettingsPageModule } from "./marketplace-settings/marketplace-settings.module";
import { TuiNotificationModule } from "@taiga-ui/core";
import { TuiLetModule } from "@taiga-ui/cdk";

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
    RouterModule.forChild(routes),
    SharedPipesModule,
    FilterPackagesPipeModule,
    MarketplaceSidebarModule,
    MarketplaceSettingsPageModule,
    ItemModule,
    TuiNotificationModule,
    TuiLetModule,
    ResponsiveColDirective,
  ],
  declarations: [MarketplaceListPage],
  exports: [MarketplaceListPage],
})
export class MarketplaceListPageModule {}
