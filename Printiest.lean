import Printiest.Tests

def main : IO Unit :=   
  IO.Prim.getStdout >>= bigSexpr2.pretty.renderStream 80
