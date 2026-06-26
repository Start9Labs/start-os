import {
  Component,
  inject,
  input,
  model,
  TemplateRef,
  viewChild,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { DialogService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiCell, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiRadioList } from '@taiga-ui/kit'
import { filter } from 'rxjs'

@Component({
  selector: '[marketplaceVersions]',
  template: `
    <span tuiTitle>
      <span tuiSubtitle>{{ 'Version' | i18n }}</span>
      {{ version() }}
    </span>
    <tui-icon icon="@tui.chevron-right" />
    <ng-template let-data="data" let-completeWith="completeWith">
      <tui-radio-list [items]="versions()" [(ngModel)]="data.version" />
      <footer class="g-buttons">
        <button tuiButton appearance="secondary" (click)="completeWith(null)">
          {{ 'Cancel' | i18n }}
        </button>
        <button
          tuiButton
          appearance="primary"
          (click)="completeWith(data.version)"
        >
          {{ 'Ok' | i18n }}
        </button>
      </footer>
    </ng-template>
  `,
  host: { '(click)': 'promptSelectVersion()' },
  hostDirectives: [TuiCell],
  imports: [TuiButton, FormsModule, TuiRadioList, i18nPipe, TuiTitle, TuiIcon],
})
export class MarketplaceVersionsComponent {
  private readonly dialog = inject(DialogService)
  private readonly template = viewChild(TemplateRef)

  readonly version = model.required<string>()
  readonly versions = input.required<string[]>()

  promptSelectVersion() {
    this.dialog
      .openComponent<string>(this.template(), {
        label: 'All versions',
        size: 's',
        data: { version: this.version() },
      })
      .pipe(filter(Boolean))
      .subscribe(selected => this.version.set(selected))
  }
}
