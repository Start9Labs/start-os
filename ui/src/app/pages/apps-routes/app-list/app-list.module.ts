import { NgModule } from "@angular/core";
import { CommonModule } from "@angular/common";
import { Routes, RouterModule } from "@angular/router";
import { IonicModule } from "@ionic/angular";
import { AppListPage } from "./app-list.page";
import { StatusComponentModule } from "src/app/components/status/status.component.module";
import { SharingModule } from "src/app/modules/sharing.module";
import { BadgeMenuComponentModule } from "src/app/components/badge-menu-button/badge-menu.component.module";
import { AppListIconComponent } from "./app-list-icon/app-list-icon.component";
import { AppListEmptyComponent } from "./app-list-empty/app-list-empty.component";
import { AppListPkgComponent } from "./app-list-pkg/app-list-pkg.component";
import { AppListRecComponent } from "./app-list-rec/app-list-rec.component";
import { AppListReorderComponent } from "./app-list-reorder/app-list-reorder.component";
import { PackageInfoPipe } from "./package-info.pipe";

const routes: Routes = [
  {
    path: "",
    component: AppListPage,
  },
];

@NgModule({
  imports: [
    CommonModule,
    StatusComponentModule,
    SharingModule,
    IonicModule,
    RouterModule.forChild(routes),
    BadgeMenuComponentModule,
  ],
  declarations: [
    AppListPage,
    AppListIconComponent,
    AppListEmptyComponent,
    AppListPkgComponent,
    AppListRecComponent,
    AppListReorderComponent,
    PackageInfoPipe,
  ],
})
export class AppListPageModule {}
