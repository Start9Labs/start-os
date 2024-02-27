import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiLoaderModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ServiceCredentialComponent } from '../components/credential.component'

@Component({
  template: `
    @if (loading$ | async) {
      <tui-loader />
    } @else {
      @for (cred of credentials | keyvalue: asIsOrder; track cred) {
        <service-credential [label]="cred.key" [value]="cred.value" />
      } @empty {
        No credentials
      }
    }
    <button tuiButton iconLeft="tuiIconRefreshCwLarge" (click)="refresh()">
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
  imports: [
    CommonModule,
    TuiButtonModule,
    ServiceCredentialComponent,
    TuiLoaderModule,
  ],
})
export class ServiceCredentialsModal {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)

  readonly id = inject<{ data: string }>(POLYMORPHEUS_CONTEXT).data
  readonly loading$ = new BehaviorSubject(true)

  credentials: Record<string, string> = {}

  async ngOnInit() {
    await this.getCredentials()
  }

  async refresh() {
    await this.getCredentials()
  }

  private async getCredentials(): Promise<void> {
    this.loading$.next(true)

    try {
      this.credentials = await this.api.getPackageCredentials({ id: this.id })
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
