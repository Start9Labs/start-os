import { AsyncPipe } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { TuiLet } from '@taiga-ui/cdk'
import { TuiButton, tuiButtonOptionsProvider } from '@taiga-ui/core'
import { map } from 'rxjs'
import { UILaunchComponent } from 'src/app/routes/portal/routes/dashboard/ui.component'
import { ActionsService } from 'src/app/services/actions.service'
import { DepErrorService } from 'src/app/services/dep-error.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from 'src/app/utils/get-package-data'

@Component({
  standalone: true,
  selector: 'fieldset[appControls]',
  template: `
    @if (pkg().status.main.status === 'running') {
      <button
        tuiIconButton
        iconStart="@tui.square"
        (click)="actions.stop(manifest())"
      >
        Stop
      </button>

      <button
        tuiIconButton
        iconStart="@tui.rotate-cw"
        (click)="actions.restart(manifest())"
      >
        Restart
      </button>
    } @else {
      <button
        *tuiLet="hasUnmet() | async as hasUnmet"
        tuiIconButton
        iconStart="@tui.play"
        [disabled]="!pkg().status.configured"
        (click)="actions.start(manifest(), !!hasUnmet)"
      >
        Start
      </button>

      <button
        tuiIconButton
        iconStart="@tui.wrench"
        (click)="actions.configure(manifest())"
      >
        Configure
      </button>
    }

    <app-ui-launch [pkg]="pkg()" />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiButton, UILaunchComponent, TuiLet, AsyncPipe],
  providers: [tuiButtonOptionsProvider({ size: 's', appearance: 'none' })],
  styles: `
    :host {
      padding: 0;
      border: none;
      cursor: default;
    }

    :host-context(tui-root._mobile) {
      button {
        display: none;
      }
    }
  `,
})
export class ControlsComponent {
  private readonly errors = inject(DepErrorService)
  readonly actions = inject(ActionsService)

  pkg = input.required<PackageDataEntry>()

  readonly manifest = computed(() => getManifest(this.pkg()))
  readonly hasUnmet = computed(() =>
    this.errors.getPkgDepErrors$(this.manifest().id).pipe(
      map(errors =>
        Object.keys(this.pkg().currentDependencies)
          .map(id => !!(errors[id] as any)?.[id]) // @TODO fix type
          .some(Boolean),
      ),
    ),
  )
}
