import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { RouterModule } from "@angular/router";
import { IonicModule } from "@ionic/angular";
import {
  EmverPipesModule,
  MarkdownPipeModule,
  SafeLinksDirective,
  SharedPipesModule,
} from "@start9labs/shared";
import { NgDompurifyModule } from "@tinkoff/ng-dompurify";

import { AboutComponent } from "./about.component";
import { DependenciesModule } from "../dependencies/dependencies.module";
import { TuiTagModule } from "@taiga-ui/kit";

@NgModule({
  imports: [
    CommonModule,
    RouterModule,
    IonicModule,
    MarkdownPipeModule,
    EmverPipesModule,
    NgDompurifyModule,
    SafeLinksDirective,
    DependenciesModule,
    SharedPipesModule,
    TuiTagModule,
  ],
  declarations: [AboutComponent],
  exports: [AboutComponent],
})
export class AboutModule {}
