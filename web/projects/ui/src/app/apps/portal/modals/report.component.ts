import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { TuiDialogContext, TuiSvgModule } from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { BackupReport } from 'src/app/services/api/api.types'

@Component({
  template: `
    <h3 class="g-title">Completed: {{ timestamp | date : 'medium' }}</h3>
    <div class="g-action">
      <div [style.flex]="1">
        <strong>System data</strong>
        <div [style.color]="system.color">{{ system.result }}</div>
      </div>
      <tui-svg [src]="system.icon" [style.color]="system.color"></tui-svg>
    </div>
    <div *ngFor="let pkg of report?.packages | keyvalue" class="g-action">
      <div [style.flex]="1">
        <strong>{{ pkg.key }}</strong>
        <div [style.color]="getColor(pkg.value.error)">
          {{ pkg.value.error ? 'Failed: ' + pkg.value.error : 'Succeeded' }}
        </div>
      </div>
      <tui-svg
        [src]="getIcon(pkg.value.error)"
        [style.color]="getColor(pkg.value.error)"
      ></tui-svg>
    </div>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiSvgModule],
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
    return error ? 'var(--tui-negative)' : 'var(--tui-positive)'
  }

  getIcon(error: unknown) {
    return error ? 'tuiIconMinusCircleLarge' : 'tuiIconCheckLarge'
  }

  private getSystem() {
    if (!this.report.server.attempted) {
      return {
        result: 'Not Attempted',
        icon: 'tuiIconMinusLarge',
        color: 'var(--tui-text-02)',
      }
    }

    if (this.report.server.error) {
      return {
        result: `Failed: ${this.report.server.error}`,
        icon: 'tuiIconMinusCircleLarge',
        color: 'var(--tui-negative)',
      }
    }

    return {
      result: 'Succeeded',
      icon: 'tuiIconCheckLarge',
      color: 'var(--tui-positive)',
    }
  }
}

export const REPORT = new PolymorpheusComponent(BackupsReportModal)
