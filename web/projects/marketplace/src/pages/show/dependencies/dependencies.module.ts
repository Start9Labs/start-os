import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { RouterModule } from "@angular/router";
import { ResponsiveColDirective, SharedPipesModule } from "@start9labs/shared";

import { DependenciesComponent } from "./dependencies.component";

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    SharedPipesModule,
    ResponsiveColDirective,
  ],
  declarations: [DependenciesComponent],
  exports: [DependenciesComponent],
})
export class DependenciesModule {}
