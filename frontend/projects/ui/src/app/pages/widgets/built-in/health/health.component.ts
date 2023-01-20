import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import {
  PackageDataEntry,
  PackageMainStatus,
} from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getPackageInfo } from '../../../../util/get-package-info'

@Component({
  selector: 'widget-health',
  templateUrl: './health.component.html',
  styleUrls: ['./health.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class HealthComponent {
  healthStates = {
    transitioning: {
      active: 0,
      label: 'Transitioning',
    },
    warning: {
      active: 0,
      label: 'Needs Attention',
    },
    error: {
      active: 0,
      label: 'Error',
    },
    healthy: {
      active: 0,
      label: 'Healthy',
    },
    stopped: {
      active: 0,
      label: 'Stopped',
    },
  }
  relevant: HealthDetails[] = []
  labels: string[] = []
  readonly data$ = inject(PatchDB)
    .watch$('package-data')
    .pipe(
      map(data => {
        const pkgs = Object.values<PackageDataEntry>(data).map(getPackageInfo)
        this.healthStates.transitioning.active = pkgs.filter(
          ({ transitioning }) => transitioning,
        ).length
        this.healthStates.error.active = pkgs.filter(
          a => a.primaryStatus !== PrimaryStatus.Stopped && a.error,
        ).length
        this.healthStates.warning.active = pkgs.filter(
          ({ warning }) => warning,
        ).length
        this.healthStates.stopped.active = pkgs.filter(
          a => a.primaryStatus === PrimaryStatus.Stopped,
        ).length
        this.healthStates.healthy.active = Math.max(
          0,
          pkgs.length -
            this.healthStates.transitioning.active -
            this.healthStates.error.active -
            this.healthStates.warning.active -
            this.healthStates.stopped.active,
        )

        this.relevant = Object.values(this.healthStates).sort((a, b) =>
          b.label.toLowerCase() > a.label.toLowerCase() ? -1 : 1,
        )
        this.labels = this.relevant.map(a => a.label)
        return this.relevant.map(a => a.active)
      }),
    )
}

function getStatus({
  installed,
}: PackageDataEntry): PackageMainStatus | undefined {
  return installed?.status.main.status
}

interface HealthDetails {
  active: number
  label: string
}
