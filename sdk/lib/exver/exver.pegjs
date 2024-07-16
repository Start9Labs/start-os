// #flavor:0.1.2-beta.1:0
// !( >=1:1 && <= 2:2)

VersionRange
  = first:VersionRangeAtom rest:(_ ((Or / And) _)? VersionRangeAtom)*

Or = "||"

And = "&&"

VersionRangeAtom
  = Parens
  / Anchor
  / Not
  / Any
  / None

Parens
  = "(" _ expr:VersionRange _ ")" { return { type: "Parens", expr } }

Anchor
  = operator:CmpOp? _ version:VersionSpec { return { type: "Anchor", operator, version } }

VersionSpec
  = flavor:Flavor? upstream:Version downstream:( ":" Version )? { return { flavor: flavor || null, upstream, downstream: downstream ? downstream[1] : { number: [0], prerelease: [] } } }

Not = "!" _ value:VersionRangeAtom { return { type: "Not", value: value }}

Any = "*" { return { type: "Any" } }

None = "!" { return { type: "None" } }

CmpOp 
  = ">=" { return ">="; }
  / "<=" { return "<="; }
  / ">" { return ">"; }
  / "<" { return "<"; }
  / "=" { return "="; }
  / "!=" { return "!="; }
  / "^" { return "^"; }
  / "~" { return "~"; }

ExtendedVersion
  = flavor:Flavor? upstream:Version ":" downstream:Version {
    return { flavor: flavor || null, upstream, downstream }
  }

Flavor
  = "#" flavor:Lowercase ":" { return flavor }

Lowercase
  = [a-z]+ { return text() }

String
  = [a-zA-Z]+ { return text(); }

Version
  = number:VersionNumber prerelease: PreRelease? {
    return { 
      number,
      prerelease: prerelease || []
    };
  }

PreRelease
  = "-" first:PreReleaseSegment rest:("." PreReleaseSegment)* {
    return [first].concat(rest.map(r => r[1]));
  }

PreReleaseSegment
  = "."? segment:(Digit / String) {
    return segment;
  }

VersionNumber
  = first:Digit rest:("." Digit)* { 
    return [first].concat(rest.map(r => r[1]));
  }

Digit
  = [0-9]+ { return parseInt(text(), 10); }

_ "whitespace"
  = [ \t\n\r]*