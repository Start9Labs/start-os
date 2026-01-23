import { ChangeDetectionStrategy, Component, computed } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiButton, TuiDialogContext } from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

import type { EthernetPort } from './service'

@Component({
  selector: 'change-wan-dialog',
  template: `
    <p>
      Select which port should become the WAN port. This will break your current
      Internet connection and restart the router.
    </p>
    <tui-radio-list
      orientation="vertical"
      [items]="portNames()"
      [(ngModel)]="selectedPort"
    />
    <footer>
      <button tuiButton appearance="flat" (click)="onCancel()">Cancel</button>
      <button tuiButton [disabled]="!canSubmit()" (click)="onConfirm()">
        Change and Restart
      </button>
    </footer>
  `,
  styles: `
    p {
      margin: 0 0 1rem;
    }

    footer {
      display: flex;
      justify-content: flex-end;
      gap: 0.5rem;
      margin-top: 1.5rem;
    }
  `,
  imports: [FormsModule, TuiButton, TuiRadioList],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ChangeWanDialog {
  private readonly context =
    injectContext<TuiDialogContext<EthernetPort | null, EthernetPort[]>>()

  readonly ports = this.context.data
  readonly currentWan = this.ports.find(p => p.wan)

  selectedPort = this.currentWan?.name || ''

  readonly portNames = computed(() => this.ports.map(p => p.name))

  canSubmit(): boolean {
    return !!this.selectedPort && this.selectedPort !== this.currentWan?.name
  }

  onCancel() {
    this.context.completeWith(null)
  }

  onConfirm() {
    const port = this.ports.find(p => p.name === this.selectedPort)
    this.context.completeWith(port || null)
  }
}

export const CHANGE_WAN_DIALOG = new PolymorpheusComponent(ChangeWanDialog)
