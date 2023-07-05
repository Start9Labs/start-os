import {
  ChangeDetectionStrategy,
  Component,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { NavController } from '@ionic/angular'
import {
  isEmptyObject,
  getPkgId,
  WithId,
  ErrorService,
  LoadingService,
} from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { filter, switchMap, timer } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  Action,
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ActionSuccessPage } from './action-success/action-success.page'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { FormPage } from 'src/app/apps/ui/modals/form/form.page'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { TUI_PROMPT } from '@taiga-ui/kit'

@Component({
  selector: 'app-actions',
  templateUrl: './app-actions.page.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsPage {
  readonly pkgId = getPkgId(this.route)
  readonly pkg$ = this.patch
    .watch$('package-data', this.pkgId)
    .pipe(filter(pkg => pkg.state === PackageState.Installed))

  constructor(
    private readonly route: ActivatedRoute,
    private readonly embassyApi: ApiService,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly loader: LoadingService,
    private readonly navCtrl: NavController,
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
      await this.embassyApi.uninstallPackage({ id: this.pkgId })
      this.embassyApi
        .setDbValue<boolean>(['ack-instructions', this.pkgId], false)
        .catch(e => console.error('Failed to mark instructions as unseen', e))
      this.navCtrl.navigateRoot('/services')
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
        id: this.pkgId,
        'action-id': actionId,
        input,
      })

      timer(500)
        .pipe(
          switchMap(() =>
            this.dialogs.open(new PolymorpheusComponent(ActionSuccessPage), {
              label: 'Execution Complete',
              data,
            }),
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

interface LocalAction {
  name: string
  description: string
  icon: string
}

@Component({
  selector: 'app-actions-item',
  templateUrl: './app-actions-item.component.html',
  styleUrls: ['./app-actions.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppActionsItemComponent {
  @Input() action!: LocalAction
}

@Pipe({
  name: 'groupActions',
})
export class GroupActionsPipe implements PipeTransform {
  transform(
    actions: PackageDataEntry['actions'],
  ): Array<Array<WithId<Action>>> | null {
    if (!actions) return null
    const noGroup = 'noGroup'
    const grouped = Object.entries(actions).reduce<
      Record<string, WithId<Action>[]>
    >((groups, [id, action]) => {
      const actionWithId = { id, ...action }
      const groupKey = action.group || noGroup
      if (!groups[groupKey]) {
        groups[groupKey] = [actionWithId]
      } else {
        groups[groupKey].push(actionWithId)
      }
      return groups
    }, {})

    return Object.values(grouped).map(group =>
      group.sort((a, b) => a.name.localeCompare(b.name)),
    )
  }
}
