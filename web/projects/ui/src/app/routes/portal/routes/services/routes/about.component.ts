import {
  ChangeDetectionStrategy,
  Component,
  inject,
  INJECTOR,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import {
  CopyService,
  DialogService,
  getPkgId,
  i18nKey,
  i18nPipe,
  MarkdownComponent,
} from '@start9labs/shared'
import { TuiCell } from '@taiga-ui/layout'
import { PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'
import {
  AdditionalItem,
  NOT_PROVIDED,
  ServiceAdditionalItemComponent,
} from '../components/additional-item.component'

@Component({
  template: `
    @for (group of groups(); track $index) {
      <section class="g-card">
        <header>{{ group.header | i18n }}</header>
        @for (item of group.items; track $index) {
          @if (item.value.startsWith('http')) {
            <a tuiCell [additionalItem]="item"></a>
          } @else {
            <button
              tuiCell
              [style.pointer-events]="!item.icon ? 'none' : null"
              [additionalItem]="item"
              (click)="item.action?.()"
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
  imports: [ServiceAdditionalItemComponent, TuiCell, i18nPipe],
})
export default class ServiceAboutRoute {
  private readonly copyService = inject(CopyService)
  private readonly markdown = inject(DialogService).openComponent(
    new PolymorpheusComponent(MarkdownComponent, inject(INJECTOR)),
    { label: 'License', size: 'l' },
  )

  readonly groups = toSignal<{ header: i18nKey; items: AdditionalItem[] }[]>(
    inject<PatchDB<DataModel>>(PatchDB)
      .watch$('packageData', getPkgId())
      .pipe(
        map(pkg => {
          const manifest = getManifest(pkg)

          return [
            {
              header: 'General',
              items: [
                {
                  name: 'Version',
                  value: manifest.version,
                  icon: '@tui.copy',
                  action: () => this.copyService.copy(manifest.version),
                },
                {
                  name: 'SDK Version',
                  value: manifest.sdkVersion || '-',
                  icon: manifest.sdkVersion ? '@tui.copy' : '',
                  action: () =>
                    manifest.sdkVersion &&
                    this.copyService.copy(manifest.sdkVersion),
                },
                {
                  name: 'Git Hash',
                  value: manifest.gitHash || '-',
                  icon: manifest.gitHash ? '@tui.copy' : '',
                  action: () =>
                    manifest.gitHash && this.copyService.copy(manifest.gitHash),
                },
                {
                  name: 'License',
                  value: manifest.license,
                  icon: '@tui.chevron-right',
                  action: () => this.markdown.subscribe(),
                },
              ],
            },
            {
              header: 'Links',
              items: [
                {
                  name: 'Installed From',
                  value: pkg.registry || NOT_PROVIDED,
                },
                {
                  name: 'Service Repository',
                  value: manifest.upstreamRepo,
                },
                {
                  name: 'Package Repository',
                  value: manifest.wrapperRepo,
                },
                {
                  name: 'Marketing Site',
                  value: manifest.marketingSite || NOT_PROVIDED,
                },
                {
                  name: 'Support Site',
                  value: manifest.supportSite || NOT_PROVIDED,
                },
                {
                  name: 'Donation Link',
                  value: manifest.donationUrl || NOT_PROVIDED,
                },
              ],
            },
          ]
        }),
      ),
  )
}
