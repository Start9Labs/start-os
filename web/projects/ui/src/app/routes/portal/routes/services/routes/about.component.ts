import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { CopyService, getPkgId, MarkdownComponent } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import {
  AdditionalItem,
  FALLBACK_URL,
  ServiceAdditionalItemComponent,
} from '../components/additional-item.component'
import { KeyValuePipe } from '@angular/common'

@Component({
  template: `
    @for (group of items() | keyvalue; track $index) {
      <section class="g-card">
        <header>{{ group.key }}</header>
        @for (additional of group.value; track $index) {
          @if (additional.description.startsWith('http')) {
            <a tuiCell [additionalItem]="additional"></a>
          } @else {
            <button
              tuiCell
              [style.pointer-events]="!additional.icon ? 'none' : null"
              [additionalItem]="additional"
              (click)="additional.action?.()"
            ></button>
          }
        }
      </section>
    }
  `,
  styles: `
    section {
      max-width: 42rem;
      display: flex;
      flex-direction: column;
      margin-bottom: 2rem;
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [ServiceAdditionalItemComponent, TuiCell, KeyValuePipe],
})
export default class ServiceAboutRoute {
  private readonly copyService = inject(CopyService)
  private readonly markdown = inject(TuiDialogService).open(
    new PolymorpheusComponent(MarkdownComponent, inject(INJECTOR)),
    { label: 'License', size: 'l' },
  )

  readonly items = toSignal<Record<string, AdditionalItem[]>>(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData', getPkgId())
      .pipe(
        map(pkg => {
          const manifest = getManifest(pkg)

          return {
            General: [
              {
                name: 'Version',
                description: manifest.version,
                icon: '@tui.copy',
                action: () => this.copyService.copy(manifest.version),
              },
              {
                name: 'Git Hash',
                description: manifest.gitHash || 'Unknown',
                icon: manifest.gitHash ? '@tui.copy' : '',
                action: () =>
                  manifest.gitHash && this.copyService.copy(manifest.gitHash),
              },
              {
                name: 'License',
                description: manifest.license,
                icon: '@tui.chevron-right',
                action: () => this.markdown.subscribe(),
              },
            ],
            Links: [
              {
                name: 'Installed From',
                description: pkg.registry || FALLBACK_URL,
              },
              {
                name: 'Service Repository',
                description: manifest.upstreamRepo,
              },
              {
                name: 'Package Repository',
                description: manifest.wrapperRepo,
              },
              {
                name: 'Marketing Site',
                description: manifest.marketingSite || FALLBACK_URL,
              },
              {
                name: 'Support Site',
                description: manifest.supportSite || FALLBACK_URL,
              },
              {
                name: 'Donation Link',
                description: manifest.donationUrl || FALLBACK_URL,
              },
            ],
          }
        }),
      ),
  )
}
