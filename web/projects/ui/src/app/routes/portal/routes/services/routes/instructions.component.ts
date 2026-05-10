import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  getPkgId,
  i18nPipe,
  MarkdownPipe,
  SafeLinksDirective,
} from '@start9labs/shared'
import { TuiNotification } from '@taiga-ui/core'
import { NgDompurifyPipe } from '@taiga-ui/dompurify'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  template: `
    @if (instructions(); as md) {
      <article
        class="instructions"
        safeLinks
        [innerHTML]="md | markdown | dompurify"
      ></article>
    } @else {
      <div tuiNotification appearance="neutral">
        {{ 'This version has no instructions. Please update.' | i18n }}
      </div>
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
    TuiNotification,
    i18nPipe,
  ],
})
export default class ServiceInstructionsRoute {
  private readonly pkgId = getPkgId()
  private readonly pkg = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('packageData', this.pkgId),
  )

  protected readonly instructions = computed(() => {
    const md = this.pkg()?.instructions?.trim()
    return md ? md : null
  })
}
