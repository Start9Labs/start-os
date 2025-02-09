export class ComposableRegex {
  readonly regex: RegExp
  constructor(regex: RegExp | string) {
    if (regex instanceof RegExp) {
      this.regex = regex
    } else {
      this.regex = new RegExp(regex)
    }
  }
  asExpr(): string {
    return `(${this.regex.source})`
  }
  matches(): string {
    return `^${this.regex.source}$`
  }
  contains(): string {
    return this.regex.source
  }
}

// https://ihateregex.io/expr/ipv6/
export const ipv6 = new ComposableRegex(
  /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/,
)

// https://ihateregex.io/expr/ipv4/
export const ipv4 = new ComposableRegex(
  /(\b25[0-5]|\b2[0-4][0-9]|\b[01]?[0-9][0-9]?)(\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}/,
)

export const hostname = new ComposableRegex(
  /(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])/,
)

export const localHostname = new ComposableRegex(
  /[-a-zA-Z0-9@:%._\+~#=]{1,256}\.local/,
)

export const torHostname = new ComposableRegex(
  /[-a-zA-Z0-9@:%._\+~#=]{1,256}\.onion/,
)

// https://ihateregex.io/expr/url/
export const url = new ComposableRegex(
  /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()!@:%_\+.~#?&\/\/=]*)/,
)

export const localUrl = new ComposableRegex(
  /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.local\b([-a-zA-Z0-9()!@:%_\+.~#?&\/\/=]*)/,
)

export const torUrl = new ComposableRegex(
  /https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.onion\b([-a-zA-Z0-9()!@:%_\+.~#?&\/\/=]*)/,
)

// https://ihateregex.io/expr/ascii/
export const ascii = new ComposableRegex(/[ -~]*/)

export const domain = new ComposableRegex(/[A-Za-z0-9.-]+\.[A-Za-z]{2,}/)

// https://www.regular-expressions.info/email.html
export const email = new ComposableRegex(`[A-Za-z0-9._%+-]+@${domain.asExpr()}`)

export const emailWithName = new ComposableRegex(
  `${email.asExpr()}|([^<]*<${email.asExpr()}>)`,
)

//https://rgxdb.com/r/1NUN74O6
export const base64 = new ComposableRegex(
  /(?:[a-zA-Z0-9+\/]{4})*(?:|(?:[a-zA-Z0-9+\/]{3}=)|(?:[a-zA-Z0-9+\/]{2}==)|(?:[a-zA-Z0-9+\/]{1}===))/,
)
