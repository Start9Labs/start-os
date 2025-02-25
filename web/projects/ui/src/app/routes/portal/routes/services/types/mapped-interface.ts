import { T } from '@start9labs/start-sdk'

export type MappedInterface = T.ServiceInterface & {
  public: boolean
  // TODO implement addresses
  addresses: any
  routerLink: string
}

export type MappedAddress = {
  name: string
  url: string
  isDomain: boolean
  isOnion: boolean
  acme: string | null
}
