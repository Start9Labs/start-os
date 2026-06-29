import { CommonModule } from '@angular/common'
import { Component, computed, inject } from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { i18nKey, i18nPipe } from '@start9labs/shared'
import { TuiDialogContext, TuiIcon } from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { T } from '@start9labs/start-core'
import { getManifest } from '../utils/get-package-data'
import { DataModel } from '../services/patch-db/data-model'

@Component({
  template: `
    <p class="timestamp">{{ data.createdAt | date: 'medium' }}</p>
    <table class="g-table">
      <thead>
        <tr>
          <th>{{ 'Title' | i18n }}</th>
          <th>{{ 'Result' | i18n }}</th>
          <th>{{ 'Duration' | i18n }}</th>
        </tr>
      </thead>
      <tbody>
        <tr>
          <td>{{ 'System data' | i18n }}</td>
          <td [style.color]="system().color">
            <tui-icon [icon]="system().icon" />
            {{ system().result | i18n }}
          </td>
          <td></td>
        </tr>
        @if (pkgTitles(); as titles) {
          @for (pkg of data.content.packages | keyvalue; track $index) {
            <tr>
              <td>{{ titles[pkg.key] || pkg.key }}</td>
              <td [style.color]="getColor(pkg.value.error)">
                <tui-icon [icon]="getIcon(pkg.value.error)" />
                {{
                  pkg.value.error
                    ? ('Failed' | i18n) + ': ' + pkg.value.error
                    : ('Succeeded' | i18n)
                }}
              </td>
              <td>{{ formatDuration(pkg.value.duration_ms) }}</td>
            </tr>
          }
        }
      </tbody>
    </table>
  `,
  styles: `
    .timestamp {
      color: var(--tui-text-secondary);
      margin: 0 0 1rem;
    }

    td:first-child {
      white-space: nowrap;
    }

    tui-icon {
      font-size: 1rem;
      vertical-align: sub;
      margin-inline-end: 0.25rem;
    }
  `,
  imports: [CommonModule, TuiIcon, i18nPipe],
})
export class BackupsReportModal {
  private readonly i18n = inject(i18nPipe)
  private readonly patch = inject<PatchDB<DataModel>>(PatchDB)

  readonly data =
    injectContext<
      TuiDialogContext<void, { content: T.BackupReport; createdAt: string }>
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

  formatDuration(ms: number): string {
    if (ms < 1000) return `${ms}ms`
    const seconds = Math.round(ms / 1000)
    if (seconds < 60) return `${seconds}s`
    const minutes = Math.floor(seconds / 60)
    const remainder = seconds % 60
    return remainder ? `${minutes}m ${remainder}s` : `${minutes}m`
  }
}

export const REPORT = new PolymorpheusComponent(BackupsReportModal)
