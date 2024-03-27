import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { Router } from '@angular/router'
import { InitializingComponent } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'

@Component({
  standalone: true,
  template: `
    <app-initializing
      [setupType]="stateService.setupType"
      (finished)="router.navigate(['success'])"
    />
  `,
  imports: [InitializingComponent],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class LoadingPage {
  readonly stateService = inject(StateService)
  readonly router = inject(Router)
}
