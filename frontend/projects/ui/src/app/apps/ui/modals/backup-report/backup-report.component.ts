import { Component, Inject } from '@angular/core'
import { BackupReport } from 'src/app/services/api/api.types'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'

@Component({
  selector: 'backup-report',
  templateUrl: './backup-report.component.html',
})
export class BackupReportComponent {
  readonly system: {
    result: string
    icon: 'remove' | 'remove-circle-outline' | 'checkmark'
    color: 'dark' | 'danger' | 'success'
  }

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<
      void,
      { report: BackupReport; timestamp: string }
    >,
  ) {
    if (!this.report.server.attempted) {
      this.system = {
        result: 'Not Attempted',
        icon: 'remove',
        color: 'dark',
      }
    } else if (this.report.server.error) {
      this.system = {
        result: `Failed: ${this.report.server.error}`,
        icon: 'remove-circle-outline',
        color: 'danger',
      }
    } else {
      this.system = {
        result: 'Succeeded',
        icon: 'checkmark',
        color: 'success',
      }
    }
  }

  get report(): BackupReport {
    return this.context.data.report
  }

  get timestamp(): string {
    return this.context.data.timestamp
  }
}
