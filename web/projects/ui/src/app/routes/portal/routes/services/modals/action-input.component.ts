import { AsyncPipe } from '@angular/common'
import { Component, inject } from '@angular/core'
import { getErrorMessage } from '@start9labs/shared'
import { T, utils } from '@start9labs/start-sdk'
import {
  TuiButton,
  TuiDialogContext,
  TuiDialogService,
  TuiLoader,
  TuiNotification,
} from '@taiga-ui/core'
import { TUI_CONFIRM, TuiConfirmData } from '@taiga-ui/kit'
import { injectContext } from '@taiga-ui/polymorpheus'
import * as json from 'fast-json-patch'
import { compare } from 'fast-json-patch'
import { PatchDB } from 'patch-db-client'
import { catchError, defer, EMPTY, endWith, firstValueFrom, map } from 'rxjs'
import {
  ActionButton,
  FormComponent,
} from 'src/app/routes/portal/components/form.component'
import { ActionRequestInfoComponent } from 'src/app/routes/portal/modals/config-dep.component'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages, getManifest } from 'src/app/utils/get-package-data'
import { InvalidService } from '../../../components/form/invalid.service'

export type PackageActionData = {
  pkgInfo: {
    id: string
    title: string
    icon: string
    mainStatus: T.MainStatus['main']
  }
  actionInfo: {
    id: string
    metadata: T.ActionMetadata
  }
  requestInfo?: T.ActionRequest
}

@Component({
  template: `
    <div class="service-title">
      <img [src]="pkgInfo.icon" alt="" />
      <h4>{{ pkgInfo.title }}</h4>
    </div>
    @if (res$ | async; as res) {
      @if (error) {
        <tui-notification appearance="negative">
          <div [innerHTML]="error"></div>
        </tui-notification>
      }

      @if (warning) {
        <tui-notification appearance="warning">
          <div [innerHTML]="warning"></div>
        </tui-notification>
      }

      @if (requestInfo) {
        <action-request-info
          [originalValue]="res.originalValue || {}"
          [operations]="res.operations || []"
        />
      }

      <app-form
        [spec]="res.spec"
        [value]="res.originalValue || {}"
        [buttons]="buttons"
        [operations]="res.operations || []"
      >
        <button
          tuiButton
          appearance="flat-grayscale"
          type="reset"
          [style.margin-right]="'auto'"
        >
          Reset Defaults
        </button>
      </app-form>
    } @else {
      <tui-loader size="l" textContent="loading" />
    }
  `,
  styles: [
    `
      tui-notification {
        font-size: 1rem;
        margin-bottom: 1.4rem;
      }
      .service-title {
        display: inline-flex;
        align-items: center;
        margin-bottom: 1.4rem;
        img {
          height: 20px;
          margin-right: 4px;
        }
        h4 {
          margin: 0;
        }
      }
    `,
  ],
  standalone: true,
  imports: [
    AsyncPipe,
    TuiNotification,
    TuiLoader,
    TuiButton,
    ActionRequestInfoComponent,
    FormComponent,
  ],
  providers: [InvalidService],
})
export class ActionInputModal {
  private readonly dialogs = inject(TuiDialogService)
  private readonly api = inject(ApiService)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)
  private readonly actionService = inject(ActionService)
  private readonly context =
    injectContext<TuiDialogContext<void, PackageActionData>>()

  readonly actionId = this.context.data.actionInfo.id
  readonly warning = this.context.data.actionInfo.metadata.warning
  readonly pkgInfo = this.context.data.pkgInfo
  readonly requestInfo = this.context.data.requestInfo

  buttons: ActionButton<any>[] = [
    {
      text: 'Submit',
      handler: value => this.execute(value),
    },
  ]

  error = ''

  res$ = defer(() =>
    this.api.getActionInput({
      packageId: this.pkgInfo.id,
      actionId: this.actionId,
    }),
  ).pipe(
    map(res => {
      const originalValue = res.value || {}

      return {
        spec: res.spec,
        originalValue,
        operations: this.requestInfo?.input
          ? compare(
              JSON.parse(JSON.stringify(originalValue)),
              utils.deepMerge(
                JSON.parse(JSON.stringify(originalValue)),
                this.requestInfo.input.value,
              ) as object,
            )
          : null,
      }
    }),
    catchError(e => {
      this.error = String(getErrorMessage(e))
      return EMPTY
    }),
  )

  async execute(input: object) {
    if (await this.checkConflicts(input)) {
      await this.actionService.execute(this.pkgInfo.id, this.actionId, input)
      this.context.$implicit.complete()
    }
  }

  private async checkConflicts(input: object): Promise<boolean> {
    const packages = await getAllPackages(this.patch)

    const breakages = Object.keys(packages)
      .filter(
        id =>
          id !== this.pkgInfo.id &&
          Object.values(packages[id].requestedActions).some(
            ({ request, active }) =>
              !active &&
              request.severity === 'critical' &&
              request.packageId === this.pkgInfo.id &&
              request.actionId === this.actionId &&
              request.when?.condition === 'input-not-matches' &&
              request.input &&
              json
                .compare(input, request.input)
                .some(op => op.op === 'add' || op.op === 'replace'),
          ),
      )
      .map(id => id)

    if (!breakages.length) return true

    const message =
      'As a result of this change, the following services will no longer work properly and may crash:<ul>'
    const content = `${message}${breakages.map(
      id => `<li><b>${getManifest(packages[id]).title}</b></li>`,
    )}</ul>`
    const data: TuiConfirmData = { content, yes: 'Continue', no: 'Cancel' }

    return firstValueFrom(
      this.dialogs.open<boolean>(TUI_CONFIRM, { data }).pipe(endWith(false)),
    )
  }
}
