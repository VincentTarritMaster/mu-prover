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

  test("check polymorphic identity 1"):
    val program = Parser.parse(SourceFile("test", "((x) => x) (! : ())"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)

  test("check polymorphic identity 1"):
    val program = Parser.parse(SourceFile("test", "([T] => (x) => x) (! : ())"))
    val (_, result) = Typer.check(program)
    assertEquals(result, Type.Unit)


  test("check polymorphic identity 2"):
    val program = Parser.parse(SourceFile("test", "[T] => (x : T) => (y : T -> T) => y : T"))
    val (_, result) = Typer.check(program)
    
    // Fa T -> ((T -> T) -> T)
    assertEquals(result, Type.ForAll(Type.Arrow(Type.Variable.Bound(0), Type.Arrow(Type.Arrow(Type.Variable.Bound(0), Type.Variable.Bound(0)), Type.Variable.Bound(0)))))

  test("check polymorphic identity 3"):
    val program = Parser.parse(SourceFile("test", "[T, U] => (x : T) => (y : T -> U) => _ : U"))
    val (_, result) = Typer.check(program)
    
    //FaFa T  -> (T -> U) -> U
    assertEquals(result, Type.ForAll(Type.ForAll(Type.Arrow(Type.Variable.Bound(1), Type.Arrow(Type.Arrow(Type.Variable.Bound(1), Type.Variable.Bound(0)), Type.Variable.Bound(0))))))

end TyperTests
