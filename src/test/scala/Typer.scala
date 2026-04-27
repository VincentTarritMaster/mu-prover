import prover.{Parser, SourceFile, Type, Typer}

final class TyperTests extends munit.FunSuite:

  test("check assertion"):
    val program = Parser.parse(SourceFile("test", "! : ()"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)

  test("check abstractrion"):
    val program = Parser.parse(SourceFile("test", "(x : ()) => x"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Arrow(Type.Unit, Type.Unit))

  test("check application"):
    val program = Parser.parse(SourceFile("test", "((x : ()) => x) (! : ())"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)

  test("check type abstraction"):
    val program = Parser.parse(SourceFile("test", "[T] => ! : T"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.ForAll(Type.Variable.Bound(0)))

  test("check type application"):
    val program = Parser.parse(SourceFile("test", "([T] => ! : T) [()]"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)

  test("check polymorphic identity"):
    val program = Parser.parse(SourceFile("test", "([T] => (x : T) => x) [()] (! : ())"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)

end TyperTests
