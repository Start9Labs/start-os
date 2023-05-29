import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { Widget } from 'src/app/services/patch-db/data-model'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'
import { BUILT_IN_WIDGETS } from '../widgets'

@Component({
  selector: 'add-widget',
  templateUrl: './add.component.html',
  styleUrls: ['./add.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AddWidgetComponent {
  readonly context = inject<TuiDialogContext<Widget>>(POLYMORPHEUS_CONTEXT)

  readonly installed$ = inject(PatchDB).watch$('ui', 'widgets')

  readonly widgets = BUILT_IN_WIDGETS

  readonly filter = (widget: Widget, installed: readonly Widget[]) =>
    !installed.find(({ id }) => id === widget.id)
}

export const ADD_WIDGET = new PolymorpheusComponent<
  AddWidgetComponent,
  TuiDialogContext<Widget>
>(AddWidgetComponent)
