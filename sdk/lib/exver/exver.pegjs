// #flavor:0.1.2-beta.1:0
// !( >=1:1 && <= 2:2)

VersionRange
  = _ expr:Or _ { return expr }

VersionSpec
  = flavor:Flavor? upstream:Version ":" downstream:Version {
    return { flavor: flavor || null, upstream, downstream }
  }

Or
  = left:And _ "||" _ right:Or { return { type: "Or", left, right} }
  / And

And
  = left:Not _ "&&" _ right:And { return { type: "And", left, right } }
  / left:Not _ right:And { return { type: "And", left, right } }
  / Not

Not
  = "!" _ value:Primary { return { type: "Not", value } }
  / Primary

Primary
  = Anchor
  / "(" _ expr:Or _ ")" { return expr }
  / "*" { return { type: "Any" } }
  / "!" { return { type: "None" } }

Anchor
  = operator:Operator _ version:VersionSpec { return { type: "Anchor", operator, version} }

Operator
  = ">=" { return ">="; }
  / "<=" { return "<="; }
  / ">" { return ">"; }
  / "<" { return "<"; }
  / "==" { return "=="; }
  / "!=" { return "!="; }

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