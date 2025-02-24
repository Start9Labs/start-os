import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { CopyService, getPkgId } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import {
  FALLBACK_URL,
  ServiceAdditionalItemComponent,
} from '../components/additional-item.component'
import ServiceMarkdownRoute from './markdown.component'

@Component({
  template: `
    <section class="g-card">
      @for (additional of items(); track $index) {
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
  `,
  styles: `
    section {
      display: flex;
      flex-direction: column;
      max-width: 32rem;
      padding: 0.75rem;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  host: { class: 'g-subpage' },
  imports: [ServiceAdditionalItemComponent, TuiCell],
})
export default class ServiceAboutRoute {
  private readonly copyService = inject(CopyService)
  private readonly markdown = inject(TuiDialogService).open(
    new PolymorpheusComponent(ServiceMarkdownRoute, inject(INJECTOR)),
    { label: 'License', size: 'l' },
  )

  readonly items = toSignal(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData', getPkgId())
      .pipe(
        map(pkg => {
          const manifest = getManifest(pkg)

          return [
            {
              name: 'Installed',
              description: new Intl.DateTimeFormat('en-US', {
                dateStyle: 'medium',
                timeStyle: 'medium',
              }).format(new Date(pkg.installedAt || 0)),
            },
            {
              name: 'Version',
              description: manifest.version,
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
            {
              name: 'Website',
              description: manifest.marketingSite || FALLBACK_URL,
            },
            {
              name: 'Donation Link',
              description: manifest.donationUrl || FALLBACK_URL,
            },
            {
              name: 'Source Repository',
              description: manifest.upstreamRepo,
            },
            {
              name: 'Support Site',
              description: manifest.supportSite || FALLBACK_URL,
            },
            {
              name: 'Registry',
              description: pkg.registry || FALLBACK_URL,
            },
            {
              name: 'Binary Source',
              description: manifest.wrapperRepo,
            },
          ]
        }),
      ),
  )
}
