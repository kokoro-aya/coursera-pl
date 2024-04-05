use "resil.lang.sml";

val a = Resil.eval (Resil.If (Resil.Logop (Resil.LE, Resil.Int (3), Resil.Int (4)), Resil.Int (3), Resil.Int ( 2)))


val b = Resil.eval (Resil.Letrec (
    [
      ("x", Resil.Int(1)),
      ("y", Resil.Int(2)),
      ("z", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Var("y")))
    ],
    Resil.Var("z")))
    
(* val c = Resil.eval (Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(7)))) *)

val c = Resil.eval (Resil.Call (Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(7))), Resil.Int(1)))


val d = Resil.eval (Resil.Snd (Resil.Pair(Resil.Int(7), Resil.Int(2))))

(*
  fun f x = x + 1
  fun g x = x * 2
  
  f(g(3)) === 7
*)
val e = Resil.eval (Resil.Call (Resil.Func ("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1))), Resil.Call(Resil.Func ("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2))), Resil.Int(3))))


val e1 = Resil.eval (Resil.Letrec (
    [
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Int(1)))),
      ("g", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.Int(2)))),
      ("z", Resil.Int(3))
    ],
    Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("g"), Resil.Var("z")))))

(*
fun double f = f . f
let val x = 4
  in (double (+2)) x
*)

val f = Resil.eval(Resil.Letrec (
  [
    ("double", Resil.Func("f", Resil.Func("#x", Resil.Call(Resil.Var("f"), Resil.Call(Resil.Var("f"), Resil.Var("#x")))))),
    ("x", Resil.Int(4))
  ],

  Resil.Call(Resil.Call(Resil.Var("double"), Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Int(2), Resil.Var("x")))), Resil.Var("x"))))


val g = Resil.eval(Resil.Letrec(
    [
      ("y", Resil.Int(3)),
      ("f", Resil.Func("x", Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("x"))))
    ],
    Resil.Call(Resil.Var("f"), Resil.Int(4))))

val g1 = Resil.eval(Resil.Letrec(
    [
      ("x", Resil.Int(3)),
      ("y", Resil.Int(4)),
      ("z", Resil.Int(5)),
      ("f", Resil.Func("x", 
              Resil.Func("y", 
                Resil.Func("z", 
                  Resil.Letrec(
                    [("w", Resil.Int(6))],
                    Resil.Binop(Resil.ADD, Resil.Var("w"),
                    Resil.Binop(Resil.ADD, Resil.Var("x"),
                    Resil.Binop(Resil.ADD, Resil.Var("y"), Resil.Var("z"))))))))
                  )
                  
    ],
    Resil.Call(Resil.Call(Resil.Call(Resil.Var("f"), Resil.Var("x")), Resil.Var("y")), Resil.Var("z"))))

(* Will fail *)
val g2 = Resil.eval(Resil.Letrec(
    [
      ("g", Resil.Letrec(
                  [("a", Resil.Int(8))],
                  Resil.Binop(Resil.ADD, Resil.Var("a"), Resil.Var("b"))
                )),
      ("f", Resil.Func("f", 
                  Resil.Letrec(
                    [("a", Resil.Int(6)),("b", Resil.Int(7))],
                    Resil.Call(Resil.Call(Resil.Var("f"), Resil.Var("a")), Resil.Var("b"))
                  )))
                  
    ],
    Resil.Call(Resil.Var("f"), Resil.Var("g"))))


(*
  class Rectangle {
    constructor (width, height) {
      this.width = width
      this.height = height
    }

    function area() {
      return this.width * this.height
    }

    function perimeter() {
      return 2 * (this.width + this.height)
    }

    function timesArea(n) {
      return n * this.area()
    }
  }

  var rect1 = new Rectangle(3, 4)
  rect1.area()

  var rect2 = new Rectangle(4, 9)
  rect2.perimeter()

  rect2.timesArea(4)
*)

val h =
Resil.show (
  Resil.eval (
    Resil.Letrec(
      [("Rectangle", 
          Resil.Func("width", 
            Resil.Func("height",
              Resil.Func("fn",
                Resil.Func("args",
                  Resil.Letrec(
                    [("area", Resil.Func("_", Resil.Binop(Resil.MULT, Resil.Var("width"), Resil.Var("height")))),
                     ("perimeter", Resil.Func("_", Resil.Binop(Resil.MULT, Resil.Int(2), Resil.Binop(Resil.ADD, Resil.Var("width"), Resil.Var("height"))))),
                     ("timesArea", Resil.Func("x", Resil.Binop(Resil.MULT, Resil.Var("x"), Resil.CallDyn(Resil.Str("area"), Resil.Unit))))
                    ],
                    Resil.CallDyn(Resil.Var("fn"), Resil.Var("args"))                        
                  ))
      )))),
      ("rect1", Resil.Call(Resil.Call(Resil.Var("Rectangle"), Resil.Int(3)), Resil.Int(4))),
      ("rect2", Resil.Call(Resil.Call(Resil.Var("Rectangle"), Resil.Int(4)), Resil.Int(9)))
      ],
        Resil.Pair(
          Resil.Pair(
            Resil.Call(Resil.Call(Resil.Var("rect1"), Resil.Str("area")), Resil.Unit),
            Resil.Call(Resil.Call(Resil.Var("rect2"), Resil.Str("perimeter")), Resil.Unit)
          ),
        Resil.Call(Resil.Call(Resil.Var("rect2"), Resil.Str("timesArea")), Resil.Int(4))
        )
      )
))

(* 
  ClosV {env=-,f=Func ("fn",Func ("args",Letrec ([(#,#)],Var "area")))}
  ClosV {env=-,f=Func ("args",Letrec ([("area",Func (#,#))],Var "area"))}
  ClosV {env=-,f=Func ("_",Binop (MULT,Var "width",Var "height"))}
*)


(*
  class Animal {
    constructor (name) {
      this.name = name
    }

    function makeSound() {
      console.log("Hello")
    }
  }

  class Cat extends Animal {
    constructor (name, color) {
      this.name = name
      this.color = color
    }

    function makeSound() {
      this.meow()
    }

    function meow() {
      console.log("Meow")
    }
  }
*)

(*
  val u = \f -> \x -> \y -> f x y

  val g x y = x + y

  u g 1 2

*)

val h1 = 
  Resil.eval (
    Resil.Letrec([
      ("u", Resil.Func("f", Resil.Func("x", Resil.Func("y", Resil.Call(Resil.Call(Resil.Var("f"), Resil.Var("x")), Resil.Var("y")))))),
      ("g", Resil.Func("x", Resil.Func("y", Resil.Binop(Resil.ADD, Resil.Var("x"), Resil.Var("y")))))
    ],
      Resil.Call(Resil.Call(Resil.Call(Resil.Var("u"), Resil.Var("g")), Resil.Int(9)), Resil.Int(4))
    )
  )

val i =

  Resil.eval (
    Resil.Letrec(
      [("Animal", 
          Resil.Func("name", 
            Resil.Func("getter",
              Resil.Letrec(
                [
                  ("getName", Resil.Func("_", Resil.Var("name")))
                ],
                  Resil.CallDyn(Resil.Var("getter"), Resil.Unit)
              )))),
      ("Cat", 
          Resil.Func("name",
            Resil.Func("color", 
              Resil.Func("getter",
                Resil.Letrec(
                  [
                    ("getName", Resil.Func("_", Resil.Var("name")))
                  ],
                    Resil.CallDyn(Resil.Var("getter"), Resil.Unit)
                ))))),
      ("Animal#name", Resil.Func("animal", Resil.Call(Resil.Var("animal"), Resil.Str("getName")))),
      ("Animal#makeSound", Resil.Func("animal", 
        Resil.Pair(
          Resil.Str("my name is"),
          Resil.Call(Resil.Var("Animal#name"), Resil.Var("animal"))))),
      ("Cat#makeSound", Resil.Func("cat", Resil.Str("meow"))),    

      ("someAnimal", Resil.Call(Resil.Var("Animal"), Resil.Str("Alex"))),
      ("someCat", Resil.Call(Resil.Call(Resil.Var("Cat"), Resil.Str("Bob")), Resil.Str("blue")))
      ],
      Resil.Pair(
      Resil.Pair(
        Resil.Call(Resil.Var("Animal#makeSound"), Resil.Var("someCat")),
        Resil.Call(Resil.Var("Cat#makeSound"), Resil.Var("someCat"))
      ),      
      Resil.Pair(
        Resil.Call(Resil.Var("Animal#name"), Resil.Var("someAnimal")),
        Resil.Call(Resil.Var("Animal#name"), Resil.Var("someCat"))
      )
      )
          
    ))



