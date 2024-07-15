import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiDialogContext, TuiIcon } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@taiga-ui/polymorpheus'
import { BackupReport } from 'src/app/services/api/api.types'

@Component({
  template: `
    <h3 class="g-title">Completed: {{ timestamp | date: 'medium' }}</h3>
    <div class="g-action">
      <div [style.flex]="1">
        <strong>System data</strong>
        <div [style.color]="system.color">{{ system.result }}</div>
      </div>
      <tui-icon [icon]="system.icon" [style.color]="system.color" />
    </div>
    <div *ngFor="let pkg of report?.packages | keyvalue" class="g-action">
      <div [style.flex]="1">
        <strong>{{ pkg.key }}</strong>
        <div [style.color]="getColor(pkg.value.error)">
          {{ pkg.value.error ? 'Failed: ' + pkg.value.error : 'Succeeded' }}
        </div>
      </div>
      <tui-icon
        [icon]="getIcon(pkg.value.error)"
        [style.color]="getColor(pkg.value.error)"
      />
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiIcon],
})
export class BackupsReportModal {
  private readonly context =
    inject<TuiDialogContext<void, { report: BackupReport; timestamp: string }>>(
      POLYMORPHEUS_CONTEXT,
    )

  readonly system = this.getSystem()

  get report(): BackupReport {
    return this.context.data.report
  }

  get timestamp(): string {
    return this.context.data.timestamp
  }

  getColor(error: unknown) {
    return error ? 'var(--tui-text-negative)' : 'var(--tui-text-positive)'
  }

  getIcon(error: unknown) {
    return error ? '@tui.circle-minus' : '@tui.check'
  }

  private getSystem() {
    if (!this.report.server.attempted) {
      return {
        result: 'Not Attempted',
        icon: '@tui.minus',
        color: 'var(--tui-text-secondary)',
      }
    }

    if (this.report.server.error) {
      return {
        result: `Failed: ${this.report.server.error}`,
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
