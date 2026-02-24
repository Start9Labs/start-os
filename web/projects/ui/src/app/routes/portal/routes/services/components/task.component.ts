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
import { ServiceTasksComponent } from 'src/app/routes/portal/routes/services/components/tasks.component'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getInstalledBaseStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'tr[task]',
  template: `
    <td tuiFade class="row">
      <tui-avatar appearance="action-grayscale" size="xs">
        <img [src]="pkg()?.icon || fallback()?.icon" alt="" />
      </tui-avatar>
      <span>{{ title() || fallback()?.title }}</span>
    </td>
    <td [style.grid-row]="2">
      <strong>
        {{
          pkg()?.actions?.[task().actionId]?.name || ('Not installed' | i18n)
        }}
      </strong>
    </td>
    <td class="row">
      @if (task().severity === 'critical') {
        <strong class="g-warning">{{ 'Required' | i18n }}</strong>
      } @else if (task().severity === 'important') {
        <strong class="g-info">{{ 'Recommended' | i18n }}</strong>
      } @else {
        <strong>{{ 'Optional' | i18n }}</strong>
      }
    </td>
    <td class="g-secondary" [style.grid-row]="3">
      {{ task().reason || ('No reason provided' | i18n) }}
    </td>
    <td>
      @if (task().severity !== 'critical') {
        <button
          tuiIconButton
          iconStart="@tui.trash"
          appearance="primary-destructive"
          [disabled]="!pkg()"
          (click)="dismiss()"
        >
          {{ 'Dismiss' | i18n }}
        </button>
      }
      <button
        tuiIconButton
        iconStart="@tui.play"
        appearance="primary-success"
        [disabled]="!pkg()"
        (click)="handle()"
      >
        {{ 'Run' | i18n }}
      </button>
    </td>
  `,
  styles: `
    td:first-child {
      white-space: nowrap;
      max-width: 15rem;
      overflow: hidden;
    }

    td:not(:last-child) {
      padding-inline-end: 1.5rem;
    }

    td:last-child {
      white-space: nowrap;
      justify-content: end;
      display: flex;
      gap: 8px;
    }

    span {
      margin-inline-start: 0.5rem;
      line-height: 1.5rem;
      vertical-align: middle;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: 1fr min-content;
      padding: 1rem 0.5rem;

      .row {
        margin-bottom: 1rem;
      }

      td {
        display: flex;
        align-items: center;
        padding: 0;
      }
    }
  `,
  host: { '[style.opacity]': 'pkg() ? null : "var(--tui-disabled-opacity)"' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, TuiAvatar, i18nPipe, TuiFade],
})
export class ServiceTaskComponent {
  private readonly actionService = inject(ActionService)
  private readonly dialog = inject(DialogService)
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly tasks = inject(ServiceTasksComponent)

  readonly task = input.required<T.Task & { replayId: string }>()
  readonly services = input.required<Record<string, PackageDataEntry>>()

  readonly pkg = computed(() => this.services()[this.task().packageId])
  readonly title = computed((pkg = this.pkg()) => pkg && getManifest(pkg).title)

  readonly fallback = computed(
    () => this.tasks.pkg().currentDependencies[this.task().packageId],
  )

  async dismiss() {
    const { packageId, replayId } = this.task()

    this.dialog
      .openConfirm(DISMISS)
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open().subscribe()
        try {
          await this.api.clearTask({ packageId, replayId, force: false })
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }

  async handle() {
    const task = this.task()
    const title = this.title()
    const pkg = this.pkg()
    const metadata = pkg?.actions[task.actionId]

    if (title && pkg && metadata) {
      this.actionService.present({
        pkgInfo: {
          id: task.packageId,
          title,
          status: getInstalledBaseStatus(pkg.statusInfo),
          icon: pkg.icon,
        },
        actionInfo: { id: task.actionId, metadata },
        prefill: task.input?.value,
      })
    }
  }
}

const DISMISS = {
  label: 'Confirm',
  size: 's',
  data: {
    content: 'Are you sure you want to dismiss this task?',
    yes: 'Dismiss',
    no: 'Cancel',
  },
} as const
