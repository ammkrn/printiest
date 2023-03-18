import Lake
open Lake DSL

package printiest {
  -- add package configuration options here
}

@[default_target]
lean_lib Printiest {
  -- add library configuration options here
}

require std from git "https://github.com/leanprover/std4" @ "main"