import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component } from '@angular/core'
import { TuiDialogContext, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'
import { BackupReport } from 'src/app/services/api/api.types'

@Component({
  template: `
    <h3 class="g-title">Completed: {{ data.createdAt | date: 'medium' }}</h3>
    <div tuiCell>
      <div tuiTitle>
        <strong>System data</strong>
        <div tuiSubtitle [style.color]="system.color">{{ system.result }}</div>
      </div>
      <tui-icon [icon]="system.icon" [style.color]="system.color" />
    </div>
    @for (pkg of data.content.packages | keyvalue; track $index) {
      <div tuiCell>
        <div tuiTitle>
          <strong>{{ pkg.key }}</strong>
          <div tuiSubtitle [style.color]="getColor(pkg.value.error)">
            {{ pkg.value.error ? 'Failed: ' + pkg.value.error : 'Succeeded' }}
          </div>
        </div>
        <tui-icon
          [icon]="getIcon(pkg.value.error)"
          [style.color]="getColor(pkg.value.error)"
        />
      </div>
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiIcon, TuiCell, TuiTitle],
})
export class BackupsReportModal {
  readonly data =
    injectContext<
      TuiDialogContext<void, { content: BackupReport; createdAt: string }>
    >().data

  readonly system = this.getSystem()

  getColor(error: unknown) {
    return error ? 'var(--tui-text-negative)' : 'var(--tui-text-positive)'
  }

  getIcon(error: unknown) {
    return error ? '@tui.circle-minus' : '@tui.check'
  }

  private getSystem() {
    if (!this.data.content.server.attempted) {
      return {
        result: 'Not Attempted',
        icon: '@tui.minus',
        color: 'var(--tui-text-secondary)',
      }
    }

    if (this.data.content.server.error) {
      return {
        result: `Failed: ${this.data.content.server.error}`,
        icon: '@tui.circle-minus',
        color: 'var(--tui-text-negative)',
      }
    }

    return {
      result: 'Succeeded',
      icon: '@tui.check',
      color: 'var(--tui-text-positive)',
    }
  }
}

export const REPORT = new PolymorpheusComponent(BackupsReportModal)
