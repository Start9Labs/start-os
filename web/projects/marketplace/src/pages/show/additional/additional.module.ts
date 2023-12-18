import { CommonModule } from "@angular/common";
import { NgModule } from "@angular/core";
import { AdditionalComponent } from "./additional.component";
import {
  TuiRadioListModule,
  TuiStringifyContentPipeModule,
} from "@taiga-ui/kit";
import { FormsModule } from "@angular/forms";
import { TuiButtonModule, TuiLabelModule } from "@taiga-ui/core";
import { AdditionalLinkModule } from "./additional-link/additional-link.component.module";
import { ResponsiveColDirective } from "@start9labs/shared";

@NgModule({
  imports: [
    CommonModule,
    ResponsiveColDirective,
    TuiRadioListModule,
    FormsModule,
    TuiStringifyContentPipeModule,
    TuiButtonModule,
    TuiLabelModule,
    AdditionalLinkModule,
  ],
  declarations: [AdditionalComponent],
  exports: [AdditionalComponent],
})
export class AdditionalModule {}