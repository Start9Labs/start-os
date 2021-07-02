import { NgModule } from '@angular/core'
import { EmverComparesPipe, EmverSatisfiesPipe, EmverDisplayPipe } from '../pipes/emver.pipe'
import { IncludesPipe } from '../pipes/includes.pipe'
import { TypeofPipe } from '../pipes/typeof.pipe'
import { MarkdownPipe } from '../pipes/markdown.pipe'
import { AnnotationStatusPipe } from '../pipes/annotation-status.pipe'
import { TruncateCenterPipe, TruncateEndPipe } from '../pipes/truncate.pipe'
import { MaskPipe } from '../pipes/mask.pipe'
import { DisplayBulbPipe } from '../pipes/display-bulb.pipe'
import { HasUiPipe, LaunchablePipe, ManifestPipe } from '../pipes/ui.pipe'
import { EmptyPipe } from '../pipes/empty.pipe'
import { StatusPipe } from '../pipes/status.pipe'
import { NotificationColorPipe } from '../pipes/notification-color.pipe'
import { ReactiveComponentModule } from '@ngrx/component'

@NgModule({
    declarations: [
        EmverComparesPipe,
        EmverSatisfiesPipe,
        TypeofPipe,
        IncludesPipe,
        MarkdownPipe,
        AnnotationStatusPipe,
        TruncateCenterPipe,
        TruncateEndPipe,
        MaskPipe,
        DisplayBulbPipe,
        EmverDisplayPipe,
        HasUiPipe,
        LaunchablePipe,
        ManifestPipe,
        EmptyPipe,
        StatusPipe,
        NotificationColorPipe,
    ],
    imports: [
      ReactiveComponentModule,
    ],
    exports: [
        EmverComparesPipe,
        EmverSatisfiesPipe,
        TypeofPipe,
        IncludesPipe,
        MarkdownPipe,
        AnnotationStatusPipe,
        TruncateEndPipe,
        TruncateCenterPipe,
        MaskPipe,
        DisplayBulbPipe,
        EmverDisplayPipe,
        HasUiPipe,
        LaunchablePipe,
        ManifestPipe,
        EmptyPipe,
        StatusPipe,
        NotificationColorPipe,
        ReactiveComponentModule,
    ],
})
export class SharingModule { }