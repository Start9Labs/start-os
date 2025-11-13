export type TunnelApi = {
  _CHILDREN: {
    auth: {
      _CHILDREN: {
        key: {
          _CHILDREN: {
            add: {
              _PARAMS: { name: string; key: AnyVerifyingKey }
              _RETURN: null
            }
            list: { _PARAMS: {}; _RETURN: { [key: string]: { name: string } } }
            remove: { _PARAMS: { key: AnyVerifyingKey }; _RETURN: null }
          }
        } & { _PARAMS: {} }
        login: {
          _PARAMS: { password: string; ephemeral: boolean }
          _RETURN: { session: string }
        }
        logout: { _PARAMS: {}; _RETURN: null | null }
        "set-password": { _PARAMS: { password: string }; _RETURN: null }
      }
    } & { _PARAMS: {} }
    db: {
      _CHILDREN: {
        apply: { _PARAMS: { expr: string; path: string | null }; _RETURN: null }
        dump: {
          _PARAMS: { pointer: string | null }
          _RETURN: { id: number; value: unknown }
        }
        subscribe: {
          _PARAMS: { pointer: string | null }
          _RETURN: { dump: { id: number; value: unknown }; guid: Guid }
        }
      }
    } & { _PARAMS: {} }
    device: {
      _CHILDREN: {
        add: {
          _PARAMS: { subnet: string; name: string; ip: string | null }
          _RETURN: null
        }
        list: {
          _PARAMS: { subnet: string }
          _RETURN: { name: string; clients: WgSubnetClients }
        }
        remove: { _PARAMS: { subnet: string; ip: string }; _RETURN: null }
        "show-config": {
          _PARAMS: { subnet: string; ip: string; wanAddr: string | null }
          _RETURN: string
        }
      }
    } & { _PARAMS: {} }
    "port-forward": {
      _CHILDREN: {
        add: { _PARAMS: { source: string; target: string }; _RETURN: null }
        remove: { _PARAMS: { source: string }; _RETURN: null }
      }
    } & { _PARAMS: {} }
    subnet: {
      _CHILDREN: {
        add: { _PARAMS: { name: string }; _RETURN: null }
        remove: { _PARAMS: {}; _RETURN: null }
      }
    } & { _PARAMS: { subnet: string } }
    web: {
      _CHILDREN: {
        disable: { _PARAMS: {}; _RETURN: null }
        enable: { _PARAMS: {}; _RETURN: null }
        "generate-certificate": {
          _PARAMS: { subject: Array<string> }
          _RETURN: string
        }
        "get-available-ips": { _PARAMS: {}; _RETURN: Array<string> }
        "get-certificate": { _PARAMS: {}; _RETURN: string | null }
        "get-listen": { _PARAMS: {}; _RETURN: string | null }
        "import-certificate": {
          _PARAMS: { key: Pem; cert: Pem }
          _RETURN: null
        }
        reset: { _PARAMS: {}; _RETURN: null }
        "set-listen": { _PARAMS: { listen: string }; _RETURN: null }
      }
    } & { _PARAMS: {} }
  }
} & { _PARAMS: {} }
