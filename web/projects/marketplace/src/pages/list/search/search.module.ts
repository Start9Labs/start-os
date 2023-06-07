import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { FormsModule } from "@angular/forms";
import { ResponsiveColDirective } from "@start9labs/shared";
import { SearchComponent } from "./search.component";

@NgModule({
  imports: [FormsModule, CommonModule, ResponsiveColDirective],
  declarations: [SearchComponent],
  exports: [SearchComponent],
})
export class SearchModule {}
