import { CommonModule } from '@angular/common'
import { Component, Inject } from '@angular/core'
import { getErrorMessage } from '@start9labs/shared'
import { T, utils } from '@start9labs/start-sdk'
import { TuiBadgeModule, TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiDialogContext,
  TuiDialogService,
  TuiLoaderModule,
  TuiModeModule,
  TuiNotificationModule,
} from '@taiga-ui/core'
import { TUI_PROMPT, TuiPromptData } from '@taiga-ui/kit'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { compare } from 'fast-json-patch'
import { PatchDB } from 'patch-db-client'
import {
  catchError,
  defer,
  EMPTY,
  endWith,
  first,
  firstValueFrom,
  map,
} from 'rxjs'
import { InvalidService } from 'src/app/components/form/invalid.service'
import { ActionRequestInfoComponent } from 'src/app/modals/action-request-input.component'
import { UiPipeModule } from 'src/app/pipes/ui/ui.module'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getAllPackages, getManifest } from 'src/app/util/get-package-data'
import * as json from 'fast-json-patch'
import { ActionService } from '../services/action.service'
import { ActionButton, FormComponent } from '../components/form.component'

export interface PackageActionData {
  readonly pkgInfo: {
    id: string
    title: string
    mainStatus: T.MainStatus['main']
  }
  readonly actionInfo: {
    id: string
    metadata: T.ActionMetadata
  }
  readonly requestInfo?: {
    dependentId?: string
    request: T.ActionRequest
  }
}

@Component({
  template: `
    <tui-badge appearance="accent">{{ pkgInfo.title }}</tui-badge>
    <ng-container *ngIf="res$ | async as res; else loading">
      <tui-notification *ngIf="error" status="error">
        <div [innerHTML]="error"></div>
      </tui-notification>

      <ng-container *ngIf="res">
        <tui-notification *ngIf="warning" status="warning">
          <div [innerHTML]="warning"></div>
        </tui-notification>

        <action-request-info
          *ngIf="requestInfo"
          [originalValue]="res.originalValue || {}"
          [operations]="res.operations || []"
        ></action-request-info>

        <app-form
          tuiMode="onDark"
          [spec]="res.spec"
          [value]="res.originalValue || {}"
          [buttons]="buttons"
          [operations]="res.operations || []"
        >
          <button
            tuiButton
            appearance="flat"
            type="reset"
            [style.margin-right]="'auto'"
          >
            Reset Defaults
          </button>
        </app-form>
      </ng-container>
    </ng-container>

    <ng-template #loading>
      <tui-loader size="l" textContent="loading"></tui-loader>
    </ng-template>
  `,
  styles: [
    `
      tui-notification {
        font-size: 1rem;
        margin-bottom: 1rem;
      }
      tui-badge {
        margin-bottom: 1rem;
      }
    `,
  ],
  standalone: true,
  imports: [
    CommonModule,
    TuiLoaderModule,
    TuiNotificationModule,
    TuiButtonModule,
    TuiModeModule,
    ActionRequestInfoComponent,
    UiPipeModule,
    FormComponent,
    TuiBadgeModule,
  ],
  providers: [InvalidService],
})
export class ActionInputModal {
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
        operations: this.requestInfo?.request.input
          ? compare(
              originalValue,
              utils.deepMerge(
                originalValue,
                this.requestInfo.request.input.value,
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

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<void, PackageActionData>,
    private readonly dialogs: TuiDialogService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
    private readonly actionService: ActionService,
  ) {}

  async execute(input: object) {
    if (await this.checkConflicts(input)) {
      return this.actionService.execute(this.pkgInfo.id, this.actionId, input)
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
    const data: TuiPromptData = { content, yes: 'Continue', no: 'Cancel' }

    return firstValueFrom(
      this.dialogs.open<boolean>(TUI_PROMPT, { data }).pipe(endWith(false)),
    )
  }
}
