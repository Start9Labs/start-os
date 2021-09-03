import { NgModule } from '@angular/core'
import { EmverComparesPipe, EmverSatisfiesPipe, EmverDisplayPipe } from '../pipes/emver.pipe'
import { IncludesPipe } from '../pipes/includes.pipe'
import { TypeofPipe } from '../pipes/typeof.pipe'
import { MarkdownPipe } from '../pipes/markdown.pipe'
import { TruncateCenterPipe, TruncateEndPipe } from '../pipes/truncate.pipe'
import { MaskPipe } from '../pipes/mask.pipe'
import { HasUiPipe, LaunchablePipe } from '../pipes/ui.pipe'
import { EmptyPipe } from '../pipes/empty.pipe'
import { NotificationColorPipe } from '../pipes/notification-color.pipe'
import { InstallState } from '../pipes/install-state.pipe'
import { TextSpinnerComponentModule } from '../components/text-spinner/text-spinner.component.module'
import { PwaBackComponentModule } from '../components/pwa-back-button/pwa-back.component.module'

@NgModule({
  declarations: [
    EmverComparesPipe,
    EmverSatisfiesPipe,
    TypeofPipe,
    IncludesPipe,
    InstallState,
    MarkdownPipe,
    TruncateCenterPipe,
    TruncateEndPipe,
    MaskPipe,
    EmverDisplayPipe,
    HasUiPipe,
    LaunchablePipe,
    EmptyPipe,
    NotificationColorPipe,
  ],
  imports: [
    TextSpinnerComponentModule,
    PwaBackComponentModule,
  ],
  exports: [
    EmverComparesPipe,
    EmverSatisfiesPipe,
    TypeofPipe,
    IncludesPipe,
    MarkdownPipe,
    TruncateEndPipe,
    TruncateCenterPipe,
    MaskPipe,
    EmverDisplayPipe,
    HasUiPipe,
    InstallState,
    LaunchablePipe,
    EmptyPipe,
    NotificationColorPipe,
    // components
    TextSpinnerComponentModule,
    PwaBackComponentModule,
  ],
})
export class SharingModule { }