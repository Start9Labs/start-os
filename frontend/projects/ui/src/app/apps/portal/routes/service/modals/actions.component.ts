import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import { Router } from '@angular/router'
import {
  isEmptyObject,
  WithId,
  ErrorService,
  LoadingService,
} from '@start9labs/shared'
import { TuiDialogContext, TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, switchMap, timer } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  Action,
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { ServiceActionComponent } from '../components/action.component'
import { ServiceActionSuccessComponent } from '../components/action-success.component'
import { GroupActionsPipe } from '../pipes/group-actions.pipe'

@Component({
  template: `
    <ng-container *ngIf="pkg$ | async as pkg">
      <section>
        <h3 class="g-title">Standard Actions</h3>
        <button
          class="g-action"
          [action]="action"
          (click)="tryUninstall(pkg)"
        ></button>
      </section>
      <ng-container *ngIf="pkg.actions | groupActions as actionGroups">
        <h3>Actions for {{ pkg.manifest.title }}</h3>
        <div *ngFor="let group of actionGroups">
          <button
            *ngFor="let action of group"
            class="g-action"
            [action]="{
              name: action.name,
              description: action.description,
              icon: 'tuiIconPlayCircleLarge'
            }"
            (click)="handleAction(action)"
          ></button>
        </div>
      </ng-container>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, ServiceActionComponent, GroupActionsPipe],
})
export class ServiceActionsModal {
  readonly pkg$ = this.patch
    .watch$('package-data', this.context.data)
    .pipe(filter(pkg => pkg.state === PackageState.Installed))

  readonly action = {
    icon: 'tuiIconTrash2Large',
    name: 'Uninstall',
    description:
      'This will uninstall the service from StartOS and delete all data permanently.',
  }

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, string>,
    private readonly embassyApi: ApiService,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly router: Router,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async handleAction(action: WithId<Action>) {
    if (action.disabled) {
      this.dialogs
        .open(action.disabled, {
          label: 'Forbidden',
          size: 's',
        })
        .subscribe()
    } else {
      if (action['input-spec'] && !isEmptyObject(action['input-spec'])) {
        this.formDialog.open(FormPage, {
          label: action.name,
          data: {
            spec: action['input-spec'],
            buttons: [
              {
                text: 'Execute',
                handler: async (value: any) =>
                  this.executeAction(action.id, value),
              },
            ],
          },
        })
      } else {
        this.dialogs
          .open(TUI_PROMPT, {
            label: 'Confirm',
            size: 's',
            data: {
              content: `Are you sure you want to execute action "${
                action.name
              }"? ${action.warning || ''}`,
              yes: 'Execute',
              no: 'Cancel',
            },
          })
          .pipe(filter(Boolean))
          .subscribe(() => this.executeAction(action.id))
      }
    }
  }

  async tryUninstall(pkg: PackageDataEntry): Promise<void> {
    const { title, alerts, id } = pkg.manifest

    let content =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (await hasCurrentDeps(this.patch, id)) {
      content = `${content}. Services that depend on ${title} will no longer work properly and may crash`
    }

    this.dialogs
      .open(TUI_PROMPT, {
        label: 'Warning',
        size: 's',
        data: {
          content,
          yes: 'Uninstall',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.uninstall())
  }

  private async uninstall() {
    const loader = this.loader.open(`Beginning uninstall...`).subscribe()

    try {
      await this.embassyApi.uninstallPackage({ id: this.context.data })
      this.embassyApi
        .setDbValue<boolean>(['ack-instructions', this.context.data], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.router.navigate(['portal', 'desktop'])
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async executeAction(
    actionId: string,
    input?: object,
  ): Promise<boolean> {
    const loader = this.loader.open('Executing action...').subscribe()

    try {
      const data = await this.embassyApi.executePackageAction({
        id: this.context.data,
        'action-id': actionId,
        input,
      })

      timer(500)
        .pipe(
          switchMap(() =>
            this.dialogs.open(
              new PolymorpheusComponent(ServiceActionSuccessComponent),
              {
                label: 'Execution Complete',
                data,
              },
            ),
          ),
        )
        .subscribe()

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  asIsOrder() {
    return 0
  }
}
