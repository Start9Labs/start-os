import { inject, Injectable } from '@angular/core'
import {
  ActivatedRouteSnapshot,
  BaseRouteReuseStrategy,
  createUrlTreeFromSnapshot,
  DetachedRouteHandle,
  UrlSerializer,
} from '@angular/router'
import { NavigationService } from './navigation.service'

@Injectable({
  providedIn: 'root',
})
export class RoutingStrategyService extends BaseRouteReuseStrategy {
  private readonly url = inject(UrlSerializer)
  private readonly navigation = inject(NavigationService)
  private readonly handlers = new Map<string, DetachedRouteHandle>()

  override shouldDetach(route: ActivatedRouteSnapshot): boolean {
    const path = this.getPath(route)
    const store = this.navigation.hasTab(path)

    if (!store) this.handlers.delete(path)

    return store
  }

  override store(
    route: ActivatedRouteSnapshot,
    handle: DetachedRouteHandle,
  ): void {
    this.handlers.set(this.getPath(route), handle)
  }

  override shouldAttach(route: ActivatedRouteSnapshot): boolean {
    return !!this.handlers.get(this.getPath(route))
  }

  override retrieve(route: ActivatedRouteSnapshot): DetachedRouteHandle | null {
    return this.handlers.get(this.getPath(route)) || null
  }

  override shouldReuseRoute(
    future: ActivatedRouteSnapshot,
    curr: ActivatedRouteSnapshot,
  ): boolean {
    // return future.routeConfig === curr.routeConfig
    // TODO: Copied from ionic for backwards compatibility, remove later
    if (future.routeConfig !== curr.routeConfig) {
      return false
    }

    // checking router params
    const futureParams = future.params
    const currentParams = curr.params
    const keysA = Object.keys(futureParams)
    const keysB = Object.keys(currentParams)
    if (keysA.length !== keysB.length) {
      return false
    }
    // Test for A's keys different from B.
    for (const key of keysA) {
      if (currentParams[key] !== futureParams[key]) {
        return false
      }
    }
    return true
  }

  private getPath(route: ActivatedRouteSnapshot): string {
    return this.url.serialize(createUrlTreeFromSnapshot(route, ['.']))
  }
}
