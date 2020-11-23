import { NgModule } from '@angular/core'
import { EmverComparesPipe, EmverSatisfiesPipe, EmverDisplayPipe, EmverIsValidPipe } from '../pipes/emver.pipe'
import { IncludesPipe } from '../pipes/includes.pipe'
import { IconPipe } from '../pipes/icon.pipe'
import { TypeofPipe } from '../pipes/typeof.pipe'
import { MarkdownPipe } from '../pipes/markdown.pipe'
import { PeekPropertiesPipe } from '../pipes/peek-properties.pipe'
import { InstalledLatestComparisonPipe, InstalledViewingComparisonPipe } from '../pipes/installed-latest-comparison.pipe'
import { AnnotationStatusPipe } from '../pipes/annotation-status.pipe'
import { TruncateCenterPipe, TruncateEndPipe } from '../pipes/truncate.pipe'
import { MaskPipe } from '../pipes/mask.pipe'
import { DisplayBulbPipe } from '../pipes/display-bulb.pipe'

@NgModule({
    declarations: [
        EmverComparesPipe,
        EmverSatisfiesPipe,
        TypeofPipe,
        IconPipe,
        IncludesPipe,
        MarkdownPipe,
        PeekPropertiesPipe,
        InstalledLatestComparisonPipe,
        InstalledViewingComparisonPipe,
        AnnotationStatusPipe,
        TruncateCenterPipe,
        TruncateEndPipe,
        MaskPipe,
        DisplayBulbPipe,
        EmverDisplayPipe,
        EmverIsValidPipe,
    ],
    exports: [
        EmverComparesPipe,
        EmverSatisfiesPipe,
        TypeofPipe,
        IconPipe,
        IncludesPipe,
        MarkdownPipe,
        PeekPropertiesPipe,
        InstalledLatestComparisonPipe,
        AnnotationStatusPipe,
        InstalledViewingComparisonPipe,
        TruncateEndPipe,
        TruncateCenterPipe,
        MaskPipe,
        DisplayBulbPipe,
        EmverDisplayPipe,
        EmverIsValidPipe,
    ],
})
export class SharingModule { }