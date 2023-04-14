import { Injectable } from '@angular/core'
import { AbstractTuiDialogService } from '@taiga-ui/cdk'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { LoadingComponent } from './loading.component'

@Injectable({ providedIn: `root` })
export class LoadingService extends AbstractTuiDialogService<unknown> {
  protected readonly component = new PolymorpheusComponent(LoadingComponent)
  protected readonly defaultOptions = {}
}
