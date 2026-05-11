import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  getPkgId,
  i18nPipe,
  MarkdownPipe,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiLoader, TuiNotification } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { catchError, defer, from, map, of, startWith } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'

type State =
  | { kind: 'loading' }
  | { kind: 'ready'; md: string }
  | { kind: 'missing' }

@Component({
  template: `
    @switch (state().kind) {
      @case ('ready') {
        <article
          class="instructions"
          safeLinks
          [innerHTML]="ready().md | markdown | dompurify"
        ></article>
      }
      @case ('missing') {
        <div tuiNotification appearance="neutral">
          {{ 'This version has no instructions. Please update.' | i18n }}
        </div>
      }
      @default {
        <tui-loader textContent="Loading" [style.height.%]="100" />
      }
    }
  `,
  styles: `
    .instructions {
      max-width: 48rem;
      line-height: 1.55;
    }

    .instructions :is(h1, h2, h3, h4) {
      margin-top: 1.5em;
      margin-bottom: 0.5em;
    }

    .instructions :is(ul, ol) {
      padding-inline-start: 1.5rem;
    }

    .instructions code {
      font-family: var(--tui-font-text-mono);
    }

    .instructions pre {
      padding: 0.75rem 1rem;
      border-radius: var(--tui-radius-m);
      background: var(--tui-background-neutral-1);
      overflow-x: auto;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    NgDompurifyPipe,
    MarkdownPipe,
    SafeLinksDirective,
    TuiLoader,
    TuiNotification,
    i18nPipe,
  ],
})
export default class ServiceInstructionsRoute {
  private readonly api = inject(ApiService)
  private readonly pkgId = getPkgId()

  protected readonly state = toSignal<State>(
    defer(() =>
      from(
        this.api.getStatic(
          [`/s9pk/installed/${this.pkgId}.s9pk/instructions.md`],
          {},
        ),
      ),
    ).pipe(
      map(md => {
        const trimmed = md?.trim()
        return trimmed
          ? ({ kind: 'ready', md: trimmed } as const)
          : ({ kind: 'missing' } as const)
      }),
      catchError(() => of({ kind: 'missing' } as const)),
      startWith({ kind: 'loading' } as const),
    ),
    { requireSync: true },
  )

  protected readonly ready = () => this.state() as { kind: 'ready'; md: string }
}
