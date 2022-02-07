import { NgModule } from '@angular/core'
import {
  EmverComparesPipe,
  EmverSatisfiesPipe,
  EmverDisplayPipe,
} from './emver.pipe'
import { IncludesPipe } from './includes.pipe'
import { TypeofPipe } from './typeof.pipe'
import { MarkdownPipe } from './markdown.pipe'
import {
  TruncateCenterPipe,
  TruncateEndPipe,
  TruncateTailPipe,
} from './truncate.pipe'
import { MaskPipe } from './mask.pipe'
import { EmptyPipe } from './empty.pipe'
import { NotificationColorPipe } from './notification-color.pipe'
import { ConvertBytesPipe } from './convert-bytes.pipe'
import { DurationToSecondsPipe } from './unit-conversion.pipe'

@NgModule({
  declarations: [
    EmverComparesPipe,
    EmverSatisfiesPipe,
    TypeofPipe,
    IncludesPipe,
    MarkdownPipe,
    TruncateCenterPipe,
    TruncateEndPipe,
    TruncateTailPipe,
    MaskPipe,
    EmverDisplayPipe,
    EmptyPipe,
    NotificationColorPipe,
    ConvertBytesPipe,
    DurationToSecondsPipe,
  ],
  exports: [
    EmverComparesPipe,
    EmverSatisfiesPipe,
    TypeofPipe,
    IncludesPipe,
    MarkdownPipe,
    TruncateEndPipe,
    TruncateCenterPipe,
    TruncateTailPipe,
    MaskPipe,
    EmverDisplayPipe,
    EmptyPipe,
    NotificationColorPipe,
    ConvertBytesPipe,
    DurationToSecondsPipe,
  ],
})
export class SharedPipesModule {}
