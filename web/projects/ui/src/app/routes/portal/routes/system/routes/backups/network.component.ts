import {
  ChangeDetectionStrategy,
  Component,
  inject,
  output,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiAlertService, TuiButton, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TUI_CONFIRM, TuiTooltip } from '@taiga-ui/kit'
import { TuiCell } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { PlaceholderComponent } from 'src/app/routes/portal/components/placeholder.component'
import { TableComponent } from 'src/app/routes/portal/components/table.component'
import { CifsBackupTarget, RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { configBuilderToSpec } from 'src/app/utils/configBuilderToSpec'
import { BackupService, MappedBackupTarget } from './backup.service'
import { BackupStatusComponent } from './status.component'

const ERROR =
  'Ensure (1) target computer is connected to the same LAN as your Start9 Server, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.'

@Component({
  standalone: true,
  selector: '[networkFolders]',
  template: `
    <header>
      Network Folders
      <tui-icon [tuiTooltip]="cifs" />
      <ng-template #cifs><ng-content /></ng-template>
      <button tuiButton size="s" iconStart="@tui.plus" (click)="add()">
        Open New
      </button>
    </header>

    <table [appTable]="['Status', 'Name', 'Hostname', 'Path', '']">
      @for (target of service.cifs(); track $index) {
        <tr
          tabindex="0"
          (click)="select(target)"
          (keydown.enter)="select(target)"
        >
          <td>
            @if (target.entry.mountable) {
              <span [backupStatus]="target.hasAnyBackup"></span>
            } @else {
              <span>
                <tui-icon
                  icon="@tui.signal-high"
                  class="g-negative"
                  [style.font-size.rem]="1"
                />
                Unable to connect
              </span>
            }
          </td>
          <td class="name">{{ target.entry.path.split('/').pop() }}</td>
          <td>{{ target.entry.hostname }}</td>
          <td>{{ target.entry.path }}</td>
          <td>
            <button
              tuiIconButton
              size="s"
              appearance="action-destructive"
              iconStart="@tui.trash"
              (click.stop)="forget(target, $index)"
            >
              Forget
            </button>
            <button
              tuiIconButton
              appearance="icon"
              size="xs"
              iconStart="@tui.pencil"
              (click.stop)="edit(target)"
            >
              Edit
            </button>
          </td>
        </tr>
      } @empty {
        <tr>
          <td colspan="5">
            <app-placeholder icon="@tui.folder-x">
              No network folders
            </app-placeholder>
          </td>
        </tr>
      }
    </table>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    tr {
      cursor: pointer;
      @include transition(background);

      @media ($tui-mouse) {
        &:hover {
          background: var(--tui-background-neutral-1-hover);
        }
      }
    }

    td:first-child {
      width: 13rem;
    }

    [tuiButton] {
      margin-inline-start: auto;
    }

    span {
      display: flex;
      align-items: center;
      gap: 0.25rem;
    }

    :host-context(tui-root._mobile) {
      tr {
        grid-template-columns: min-content 1fr 4rem;
        white-space: nowrap;
      }

      td {
        grid-column: span 2;

        &:first-child {
          font-size: 0;
          width: auto;
          grid-area: 1 / 2;
          place-content: center;
          margin: 0 0.5rem;
        }

        &:last-child {
          grid-area: 1 / 3 / 4 / 3;
          align-self: center;
          justify-self: end;
        }
      }

      .name {
        color: var(--tui-text-primary);
        font: var(--tui-font-text-m);
        font-weight: bold;
        grid-column: 1;
        max-width: 12rem;
      }
    }
  `,
  host: { class: 'g-card' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    TuiButton,
    TuiIcon,
    TuiTooltip,
    PlaceholderComponent,
    BackupStatusComponent,
    TableComponent,
  ],
})
export class BackupNetworkComponent {
  private readonly dialogs = inject(TuiResponsiveDialogService)
  private readonly alerts = inject(TuiAlertService)
  private readonly formDialog = inject(FormDialogService)
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly type = inject(ActivatedRoute).snapshot.data['type']

  readonly service = inject(BackupService)
  readonly networkFolders = output<MappedBackupTarget<CifsBackupTarget>>()

  select(target: MappedBackupTarget<CifsBackupTarget>) {
    if (!target.entry.mountable) {
      this.alerts
        .open(ERROR, {
          appearance: 'negative',
          label: 'Unable to connect',
          autoClose: 0,
        })
        .subscribe()
    } else if (this.type === 'restore' && !target.hasAnyBackup) {
      this.alerts
        .open('Network Folder does not contain a valid backup', {
          appearance: 'negative',
        })
        .subscribe()
    } else {
      this.networkFolders.emit(target)
    }
  }

  async add() {
    this.formDialog.open(FormComponent, {
      label: 'New Network Folder',
      data: {
        spec: await configBuilderToSpec(cifsSpec),
        buttons: [
          {
            text: 'Execute',
            handler: (value: RR.AddBackupTargetReq) => this.addTarget(value),
          },
        ],
      },
    })
  }

  async edit(target: MappedBackupTarget<CifsBackupTarget>) {
    this.formDialog.open(FormComponent, {
      label: 'Update Network Folder',
      data: {
        spec: await configBuilderToSpec(cifsSpec),
        buttons: [
          {
            text: 'Execute',
            handler: async (value: RR.AddBackupTargetReq) => {
              const loader = this.loader
                .open('Testing connectivity to shared folder...')
                .subscribe()

              try {
                const res = await this.api.updateBackupTarget({
                  id: target.id,
                  ...value,
                })

                target.entry = Object.values(res)[0]
                this.service.cifs.update(cifs => [...cifs])
                return true
              } catch (e: any) {
                this.errorService.handleError(e)
                return false
              } finally {
                loader.unsubscribe()
              }
            },
          },
        ],
        value: { ...target.entry },
      },
    })
  }

  forget({ id }: MappedBackupTarget<CifsBackupTarget>, index: number) {
    this.dialogs
      .open(TUI_CONFIRM, { label: 'Are you sure?', size: 's' })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Removing...').subscribe()

        try {
          await this.api.removeBackupTarget({ id })
          this.service.cifs.update(cifs => cifs.filter((_, i) => i !== index))
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  private async addTarget(v: RR.AddBackupTargetReq): Promise<boolean> {
    const loader = this.loader
      .open('Testing connectivity to shared folder...')
      .subscribe()

    try {
      const [[id, entry]] = Object.entries(await this.api.addBackupTarget(v))
      const hasAnyBackup = this.service.hasAnyBackup(entry)
      const added = { id, entry, hasAnyBackup }
      this.service.cifs.update(cifs => [added, ...cifs])
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}

const cifsSpec = ISB.InputSpec.of({
  hostname: ISB.Value.text({
    name: 'Hostname',
    description:
      'The hostname of your target device on the Local Area Network.',
    warning: null,
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    required: true,
    default: null,
    patterns: [],
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: `On Windows, this is the fully qualified path to the shared folder, (e.g. /Desktop/my-folder).\n\n On Linux and Mac, this is the literal name of the shared folder (e.g. my-shared-folder).`,
    placeholder: 'e.g. my-shared-folder or /Desktop/my-folder',
    required: true,
    default: null,
  }),
  username: ISB.Value.text({
    name: 'Username',
    description: `On Linux, this is the samba username you created when sharing the folder.\n\n On Mac and Windows, this is the username of the user who is sharing the folder.`,
    required: true,
    default: null,
    placeholder: 'My Network Folder',
  }),
  password: ISB.Value.text({
    name: 'Password',
    description: `On Linux, this is the samba password you created when sharing the folder.\n\n On Mac and Windows, this is the password of the user who is sharing the folder.`,
    required: false,
    default: null,
    masked: true,
    placeholder: 'My Network Folder',
  }),
})
