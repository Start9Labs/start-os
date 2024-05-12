import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ActivatedRoute, Router } from '@angular/router'
import {
  isEmptyObject,
  WithId,
  ErrorService,
  LoadingService,
  getPkgId,
} from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { filter, switchMap, timer } from 'rxjs'
import { FormComponent } from 'src/app/routes/portal/components/form.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { hasCurrentDeps } from 'src/app/utils/has-deps'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ServiceActionComponent } from '../components/action.component'
import { ServiceActionSuccessComponent } from '../components/action-success.component'
import { GroupActionsPipe } from '../pipes/group-actions.pipe'
import { ToManifestPipe } from 'src/app/routes/portal/pipes/to-manifest'
import { T } from '@start9labs/start-sdk'
import { getAllPackages, getManifest } from 'src/app/utils/get-package-data'

@Component({
  template: `
    @if (pkg$ | async; as pkg) {
      <section>
        <h3 class="g-title">Standard Actions</h3>
        <button
          class="g-action"
          [action]="action"
          (click)="tryUninstall(pkg)"
        ></button>
      </section>
      <ng-container *ngIf="pkg.actions | groupActions as actionGroups">
        <h3 *ngIf="actionGroups.length" class="g-title">
          Actions for {{ (pkg | toManifest).title }}
        </h3>
        <div *ngFor="let group of actionGroups">
          <button
            *ngFor="let action of group"
            class="g-action"
            [action]="{
              name: action.name,
              description: action.description,
              icon: 'tuiIconPlayCircle'
            }"
            (click)="handleAction(action)"
          ></button>
        </div>
      </ng-container>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    ServiceActionComponent,
    GroupActionsPipe,
    ToManifestPipe,
  ],
})
export class ServiceActionsRoute {
  private readonly id = getPkgId(inject(ActivatedRoute))

  readonly pkg$ = this.patch
    .watch$('packageData', this.id)
    .pipe(filter(pkg => pkg.stateInfo.state === 'installed'))

  readonly action = {
    icon: 'tuiIconTrash2',
    name: 'Uninstall',
    description:
      'This will uninstall the service from StartOS and delete all data permanently.',
  }

  constructor(
    private readonly embassyApi: ApiService,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly router: Router,
    private readonly patch: PatchDB<DataModel>,
    private readonly formDialog: FormDialogService,
  ) {}

  async handleAction(action: WithId<T.ActionMetadata>) {
    if (action.disabled) {
      this.dialogs
        .open(action.disabled, {
          label: 'Forbidden',
          size: 's',
        })
        .subscribe()
    } else {
      if (action.input && !isEmptyObject(action.input)) {
        this.formDialog.open(FormComponent, {
          label: action.name,
          data: {
            spec: action.input,
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
    const { title, alerts, id } = getManifest(pkg)

    let content =
      alerts.uninstall ||
      `Uninstalling ${title} will permanently delete its data`

    if (hasCurrentDeps(id, await getAllPackages(this.patch))) {
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
      await this.embassyApi.uninstallPackage({ id: this.id })
      this.embassyApi
        .setDbValue<boolean>(['ack-instructions', this.id], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.router.navigate(['./portal/dashboard'])
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
        id: this.id,
        actionId,
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
