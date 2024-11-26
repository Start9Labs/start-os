import { TuiLoader, TuiButton } from '@taiga-ui/core'
import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { POLYMORPHEUS_CONTEXT } from '@taiga-ui/polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServicePropertyComponent } from '../components/property.component'

@Component({
  template: `
    @if (loading$ | async) {
      <tui-loader />
    } @else {
      @for (prop of properties | keyvalue: asIsOrder; track prop) {
        <service-property [label]="prop.key" [value]="prop.value" />
      } @empty {
        No properties
      }
    }
    <button tuiButton iconStart="@tui.refresh-cw" (click)="refresh()">
      Refresh
    </button>
  `,
  styles: [
    `
      button {
        float: right;
        margin-top: 1rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, TuiButton, ServicePropertyComponent, TuiLoader],
})
export class ServicePropertiesModal {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  readonly id = inject<{ data: string }>(POLYMORPHEUS_CONTEXT).data
  readonly loading$ = new BehaviorSubject(true)

  properties: Record<string, string> = {}

  async ngOnInit() {
    await this.getProperties()
  }

  async refresh() {
    await this.getProperties()
  }

  private async getProperties(): Promise<void> {
    this.loading$.next(true)

    try {
      // @TODO Matt this needs complete rework, right?
      // this.properties = await this.api.getPackageProperties({ id: this.id })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
