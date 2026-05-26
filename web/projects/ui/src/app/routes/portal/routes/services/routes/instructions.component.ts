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

@Component({
  template: `
    @if (value() === undefined) {
      <tui-loader textContent="Loading" [style.height.%]="100" />
    } @else if (value(); as value) {
      <article safeLinks [innerHTML]="value | markdown | dompurify"></article>
    } @else {
      <div tuiNotification appearance="neutral">
        {{ 'This version has no instructions. Please update.' | i18n }}
      </div>
    }
  `,
  styles: `
    article {
      max-width: 48rem;
      line-height: 1.55;

      ::ng-deep :is(h1, h2, h3, h4) {
        margin: 1.5em 0 0.5em;

        &:first-child {
          margin-top: 0;
        }
      }

      ::ng-deep :is(ul, ol) {
        padding-inline-start: 1rem;
      }

      ::ng-deep pre {
        padding: 0.75rem 1rem;
        border-radius: var(--tui-radius-m);
        background: var(--tui-background-neutral-1);
        overflow-x: auto;
      }

      ::ng-deep table {
        inline-size: 100%;
        border-collapse: collapse;
      }

      ::ng-deep thead {
        background: var(--tui-background-neutral-1);
      }

      ::ng-deep :is(td, th) {
        border: 1px solid var(--tui-border-normal);
        padding: 0.25rem 0.75rem;
      }
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
  protected readonly value = toSignal(
    from(
      inject(ApiService).getStatic(
        [`/s9pk/installed/${getPkgId()}.s9pk/instructions.md`],
        {},
      ),
    ).pipe(
      map(md => md?.trim()),
      catchError(() => of('')),
    ),
  )
}
