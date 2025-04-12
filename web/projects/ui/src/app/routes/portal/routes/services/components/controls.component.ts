import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  Input,
} from '@angular/core'
import { TuiButton } from '@taiga-ui/core'
import { map } from 'rxjs'
import { ControlsService } from 'src/app/services/controls.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PrimaryStatus } from 'src/app/services/pkg-status-rendering.service'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  selector: 'service-controls',
  template: `
    @if (status && ['running', 'starting', 'restarting'].includes(status)) {
      <button
        tuiButton
        appearance="primary-destructive"
        iconStart="@tui.square"
        (click)="controls.stop(manifest())"
      >
        Stop
      </button>
    }

    @if (status === 'running') {
      <button
        tuiButton
        iconStart="@tui.rotate-cw"
        (click)="controls.restart(manifest())"
      >
        Restart
      </button>
    }

    @if (status === 'stopped') {
      <button
        tuiButton
        iconStart="@tui.play"
        (click)="controls.start(manifest(), !!hasUnmet())"
      >
        Start
      </button>
    }
  `,
  styles: [
    `
      :host {
        width: 100%;
        max-width: 18rem;
        display: flex;
        flex-wrap: wrap;
        gap: 1rem;
        justify-content: center;
        margin-block-start: 1rem;

        &:nth-child(3) {
          grid-row: span 2;
        }
      }

      [tuiButton] {
        flex: 1;
        min-width: fit-content;
      }

      :host-context(tui-root._mobile) {
        display: flex;
        margin: 0;

        [tuiButton] {
          font-size: 0;
          gap: 0;
          border-radius: 100%;
        }
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton],
})
export class ServiceControlsComponent {
  private readonly errors = inject(DepErrorService)

  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input({ required: true })
  status?: PrimaryStatus

  readonly manifest = computed(() => getManifest(this.pkg))

  readonly controls = inject(ControlsService)

  // @TODO Alex observable in signal?
  readonly hasUnmet = computed(() =>
    this.errors.getPkgDepErrors$(this.manifest().id).pipe(
      map(errors =>
        Object.keys(this.pkg.currentDependencies)
          .map(id => errors[id])
          .some(Boolean),
      ),
    ),
  )
}
