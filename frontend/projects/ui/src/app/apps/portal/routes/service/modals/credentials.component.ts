import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService, SharedPipesModule } from '@start9labs/shared'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiButtonModule } from '@taiga-ui/core'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { SkeletonListComponentModule } from 'src/app/common/skeleton-list/skeleton-list.component.module'
import { ServiceCredentialComponent } from '../components/credential.component'

@Component({
  template: `
    <skeleton-list *ngIf="loading$ | async; else loaded"></skeleton-list>
    <ng-template #loaded>
      <service-credential
        *ngFor="let cred of credentials | keyvalue : asIsOrder; empty: blank"
        [label]="cred.key"
        [value]="cred.value"
      ></service-credential>
    </ng-template>
    <ng-template #blank>No credentials</ng-template>
    <button tuiButton icon="tuiIconRefreshCwLarge" (click)="refresh()">
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
    TuiForModule,
    TuiButtonModule,
    SharedPipesModule,
    SkeletonListComponentModule,
    ServiceCredentialComponent,
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
