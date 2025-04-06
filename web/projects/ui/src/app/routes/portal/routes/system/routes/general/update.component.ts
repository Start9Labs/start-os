import { TuiAutoFocus } from '@taiga-ui/cdk'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, Inject } from '@angular/core'
import {
  ErrorService,
  LoadingService,
  MarkdownPipe,
  SafeLinksDirective,
} from '@start9labs/shared'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { TuiDialogContext, TuiScrollbar, TuiButton } from '@taiga-ui/core'
import { NgDompurifyModule } from '@tinkoff/ng-dompurify'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { EOSService } from 'src/app/services/eos.service'

@Component({
  template: `
    <h2 style="margin-top: 0">StartOS {{ versions[0].version }}</h2>
    <h3 style="color: var(--tui-text-secondary); font-weight: normal">
      Release Notes
    </h3>
    <tui-scrollbar style="margin-bottom: 24px; max-height: 50vh;">
      @for (v of versions; track $index) {
        <h4 class="g-title">{{ v.version }}</h4>
        <div safeLinks [innerHTML]="v.notes | markdown | dompurify"></div>
      }
    </tui-scrollbar>
    <button tuiButton tuiAutoFocus style="float: right;" (click)="update()">
      Begin Update
    </button>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    MarkdownPipe,
    NgDompurifyModule,
    SafeLinksDirective,
    TuiAutoFocus,
    TuiButton,
    TuiScrollbar,
  ],
})
export class SystemUpdateModal {
  readonly versions = Object.entries(this.eosService.osUpdate?.releaseNotes!)
    .sort(([a], [b]) => a.localeCompare(b))
    .reverse()
    .map(([version, notes]) => ({
      version,
      notes,
    }))

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT) private readonly context: TuiDialogContext,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly eosService: EOSService,
  ) {}

  async update() {
    const loader = this.loader.open('Beginning update...').subscribe()

    try {
      await this.embassyApi.updateServer()
      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

export const UPDATE = new PolymorpheusComponent(SystemUpdateModal)
