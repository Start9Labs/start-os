import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  ErrorService,
  i18nPipe,
  LoadingService,
  MarkdownPipe,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiAutoFocus } from '@taiga-ui/cdk'
import { TuiButton, TuiDialogContext, TuiScrollbar } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { firstValueFrom } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { OSService } from 'src/app/services/os.service'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    <h2 style="margin-top: 0">StartOS {{ versions[0]?.version }}</h2>
    <h3 style="color: var(--tui-text-secondary); font-weight: normal">
      {{ 'Release notes' | i18n }}
    </h3>
    <tui-scrollbar style="margin-bottom: 24px; max-height: 50vh;">
      @for (v of versions; track $index) {
        <h4 class="g-title">{{ v.version }}</h4>
        <div safeLinks [innerHTML]="v.notes | markdown | dompurify"></div>
      }
    </tui-scrollbar>
    <button tuiButton tuiAutoFocus style="float: right;" (click)="update()">
      {{ 'Begin Update' | i18n }}
    </button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    MarkdownPipe,
    NgDompurifyPipe,
    SafeLinksDirective,
    TuiAutoFocus,
    TuiButton,
    TuiScrollbar,
    i18nPipe,
  ],
})
export class SystemUpdateModal {
  readonly versions = Object.entries(this.os.osUpdate!)
    .sort(([a], [b]) => a.localeCompare(b))
    .reverse()
    .map(([version, info]) => ({
      version,
      notes: info.releaseNotes,
    }))

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT) private readonly context: TuiDialogContext,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly os: OSService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async update() {
    const loader = this.loader.open('Beginning update').subscribe()

    const { startosRegistry } = await firstValueFrom(this.patch.watch$('ui'))

    try {
      await this.embassyApi.updateServer({
        targetVersion: `=${this.versions[0]!.version}`,
        registry: startosRegistry,
      })
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

export const UPDATE = new PolymorpheusComponent(SystemUpdateModal)
