import { Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiDialogContext, TuiTextfield } from '@taiga-ui/core'
import { TuiDataListWrapper, TuiSelect } from '@taiga-ui/kit'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { StartOSDiskInfoWithId } from '../types'

interface Data {
  servers: StartOSDiskInfoWithId[]
}

@Component({
  standalone: true,
  imports: [FormsModule, TuiTextfield, TuiSelect, TuiDataListWrapper],
  template: `
    <p>Multiple backups found. Select which one to restore.</p>
    <tui-textfield [stringify]="stringify">
      <label tuiLabel>Backups</label>
      <input tuiSelect [(ngModel)]="selectedServer" />
      <tui-data-list-wrapper
        new
        *tuiTextfieldDropdown
        [items]="context.data.servers"
        [itemContent]="serverContent"
      />
    </tui-textfield>

    <ng-template #serverContent let-server>
      <div class="server-item">
        <span>{{ server.id }}</span>
        <!-- @TODO eos-version? -->
        <small>{{ server['eos-version'] }}</small>
      </div>
    </ng-template>
  `,
  styles: `
    .server-item {
      display: flex;
      flex-direction: column;

      small {
        opacity: 0.7;
      }
    }
  `,
})
export class SelectNetworkBackupDialog {
  protected readonly context =
    injectContext<TuiDialogContext<StartOSDiskInfoWithId | null, Data>>()

  private _selectedServer: StartOSDiskInfoWithId | null = null

  get selectedServer(): StartOSDiskInfoWithId | null {
    return this._selectedServer
  }

  set selectedServer(value: StartOSDiskInfoWithId | null) {
    this._selectedServer = value

    if (value) {
      this.context.completeWith(value)
    }
  }

  readonly stringify = (server: StartOSDiskInfoWithId | null) =>
    server ? server.id : ''
}

export const SELECT_NETWORK_BACKUP = new PolymorpheusComponent(
  SelectNetworkBackupDialog,
)
