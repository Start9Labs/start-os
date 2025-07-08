import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import {
  DialogService,
  ErrorService,
  i18nPipe,
  LoadingService,
} from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiButton } from '@taiga-ui/core'
import { TuiAvatar, TuiFade } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'tr[task]',
  template: `
    <td tuiFade>
      <tui-avatar size="xs"><img [src]="pkg()?.icon" alt="" /></tui-avatar>
      <span>{{ pkgTitle() }}</span>
    </td>
    <td>
      {{ pkg()?.actions?.[task().actionId]?.name }}
    </td>
    <td>
      @if (task().severity === 'critical') {
        <strong [style.color]="'var(--tui-status-warning)'">
          {{ 'Required' | i18n }}
        </strong>
      } @else if (task().severity === 'important') {
        <strong [style.color]="'var(--tui-status-info)'">
          {{ 'Recommended' | i18n }}
        </strong>
      } @else {
        <strong>
          {{ 'Optional' | i18n }}
        </strong>
      }
    </td>
    <td
      [style.color]="'var(--tui-text-secondary)'"
      [style.grid-area]="'2 / span 4'"
    >
      {{ task().reason || ('No reason provided' | i18n) }}
    </td>
    <td>
      @if (task().severity !== 'critical') {
        <button
          tuiIconButton
          iconStart="@tui.trash"
          appearance="flat-grayscale"
          (click)="dismiss()"
        ></button>
      }
      <button
        tuiIconButton
        iconStart="@tui.play"
        appearance="flat-grayscale"
        (click)="handle()"
      ></button>
    </td>
  `,
  styles: `
    td:first-child {
      white-space: nowrap;
      max-width: 15rem;
      overflow: hidden;
    }

    td:last-child {
      grid-area: 3 / span 4;
      white-space: nowrap;
      text-align: right;
      flex-direction: row-reverse;
      justify-content: flex-end;
      gap: 0.5rem;
    }

    span {
      margin-inline-start: 0.5rem;
      vertical-align: middle;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      align-items: center;
      padding: 1rem 0rem 1rem 0.5rem;
      gap: 0.5rem;

      td {
        display: flex;
        align-items: center;
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiAvatar, i18nPipe, TuiFade],
})
export class ServiceTaskComponent {
  private readonly actionService = inject(ActionService)
  private readonly dialog = inject(DialogService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)

  readonly task = input.required<T.Task & { replayId: string }>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly pkg = computed(() => this.services()[this.task().packageId])
  readonly pkgTitle = computed(
    (pkg = this.pkg()) => pkg && getManifest(pkg).title,
  )

  async dismiss() {
    this.dialog
      .openConfirm<boolean>({
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Are you sure you want to dismiss this task?',
          yes: 'Dismiss',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open().subscribe()
        try {
          await this.api.clearTask({
            packageId: this.task().packageId,
            replayId: this.task().replayId,
          })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async handle() {
    const title = this.pkgTitle()
    const pkg = this.pkg()
    const metadata = pkg?.actions[this.task().actionId]

    if (!title || !pkg || !metadata) {
      return
    }

    this.actionService.present({
      pkgInfo: {
        id: this.task().packageId,
        title,
        mainStatus: pkg.status.main,
        icon: pkg.icon,
      },
      actionInfo: {
        id: this.task().actionId,
        metadata,
      },
      requestInfo: this.task(),
    })
  }
}
