import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiDialogContext, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { BackupReport } from 'src/app/services/api/api.types'
import { getManifest } from '../utils/get-package-data'
import { DataModel } from '../services/patch-db/data-model'

@Component({
  template: `
    <h3 class="g-title">
      {{ 'Completed' | i18n }}: {{ data.createdAt | date: 'medium' }}
    </h3>
    <div tuiCell>
      <div tuiTitle>
        <strong>{{ 'System data' | i18n }}</strong>
        <div tuiSubtitle [style.color]="system().color">
          {{ system().result | i18n }}
        </div>
      </div>
      <tui-icon [icon]="system().icon" [style.color]="system().color" />
    </div>
    @if (pkgTitles(); as titles) {
      @for (pkg of data.content.packages | keyvalue; track $index) {
        <div tuiCell>
          <div tuiTitle>
            <strong>{{ titles[pkg.key] || pkg.key }}</strong>
            <div tuiSubtitle [style.color]="getColor(pkg.value.error)">
              {{
                pkg.value.error
                  ? ('Failed' | i18n) + ': ' + pkg.value.error
                  : ('Succeeded' | i18n)
              }}
            </div>
          </div>
          <tui-icon
            [icon]="getIcon(pkg.value.error)"
            [style.color]="getColor(pkg.value.error)"
          />
        </div>
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [CommonModule, TuiIcon, TuiCell, TuiTitle, i18nPipe],
})
export class BackupsReportModal {
  private readonly i18n = inject(i18nPipe)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly data =
    injectContext<
      TuiDialogContext<void, { content: BackupReport; createdAt: string }>
    >().data

  readonly pkgTitles = toSignal(
    this.patch.watch$('packageData').pipe(
      map(allPkgs =>
        Object.values(allPkgs).reduce<Record<string, string>>((acc, pkg) => {
          const { id, title } = getManifest(pkg)
          return {
            ...acc,
            [id]: title,
          }
        }, {}),
      ),
    ),
  )

  readonly system = computed(
    (): { result: i18nKey; icon: string; color: string } => {
      if (!this.data.content.server.attempted) {
        return {
          result: 'Not attempted',
          icon: '@tui.minus',
          color: 'var(--tui-text-secondary)',
        }
      }

      if (this.data.content.server.error) {
        return {
          result:
            `${this.i18n.transform('Failed')}: ${this.data.content.server.error}` as i18nKey,
          icon: '@tui.circle-minus',
          color: 'var(--tui-text-negative)',
        }
      }

      return {
        result: 'Succeeded',
        icon: '@tui.check',
        color: 'var(--tui-text-positive)',
      }
    },
  )

  getColor(error: unknown) {
    return error ? 'var(--tui-text-negative)' : 'var(--tui-text-positive)'
  }

  getIcon(error: unknown) {
    return error ? '@tui.circle-minus' : '@tui.check'
  }
}

export const REPORT = new PolymorpheusComponent(BackupsReportModal)
