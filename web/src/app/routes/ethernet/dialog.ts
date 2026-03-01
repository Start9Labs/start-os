import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiButton, TuiDialogContext, TuiTitle } from '@taiga-ui/core'
import {
  TuiRadioList,
  TuiStringifyContentPipe,
  TuiStringifyPipe,
} from '@taiga-ui/kit'
import { TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { provideHelp } from 'src/app/help/help'
import { ModalHelp } from 'src/app/help/modal-help'

import type { EthernetPort } from './service'

@Component({
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2>Change WAN Port</h2>
        <p>
          Select which port should become the WAN port. This will break your
          current Internet connection and restart the router.
        </p>
      </hgroup>
    </header>
    <tui-radio-list
      [itemContent]="'name' | tuiStringify | tuiStringifyContent"
      [items]="context.data"
      [(ngModel)]="selected"
    />
    <footer>
      <button
        tuiButton
        appearance="flat"
        (click)="context.$implicit.complete()"
      >
        Cancel
      </button>
      <button
        tuiButton
        [disabled]="!current || current === this.selected"
        (click)="context.completeWith(selected)"
      >
        Change and Restart
      </button>
    </footer>
  `,
  hostDirectives: [ModalHelp],
  providers: [provideHelp('/ethernet/dialog')],
  imports: [
    FormsModule,
    TuiButton,
    TuiRadioList,
    TuiHeader,
    TuiTitle,
    TuiStringifyPipe,
    TuiStringifyContentPipe,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ChangeWanDialog {
  readonly context =
    injectContext<TuiDialogContext<EthernetPort | undefined, EthernetPort[]>>()

  current = this.context.data.find(p => p.wan)
  selected = this.current
}

export const CHANGE_WAN_DIALOG = new PolymorpheusComponent(ChangeWanDialog)
