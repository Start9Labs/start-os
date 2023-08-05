import { Subscription } from 'rxjs'
export abstract class RegistrySettingsComponent {
  abstract connect(url: string, loader?: Subscription): Promise<void>
}
