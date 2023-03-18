import Printiest.Tests

def main : IO Unit := do
  (IO.getStdout >>= bigSexpr2.pretty.renderStreamB 80) |>.toIO
