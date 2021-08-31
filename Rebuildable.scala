package rebuildable

import annotation.implicitNotFound

// Implementation

object Rebuildable:
  @implicitNotFound("Object ${T} is past it's rebuilding phase.")
  class BuildPhase[T] private[Rebuildable]() // Has to be invariant (ideally erased when that drops)
  opaque type BeingBuilt = Any // To protect from mutating caller self type

  extension (self: Rebuildable)
    def having(inits: (BuildPhase[self.type & BeingBuilt] ?=> self.type & BeingBuilt => Unit)*): self.type =
      val clone: self.type & BeingBuilt = self.cloned.asInstanceOf
      inits.foreach { init => init(using BuildPhase[clone.type])(clone) }
      clone

trait Rebuildable extends Cloneable:
  type BuildPhase = Rebuildable.BuildPhase[this.type]
  def cloned: this.type = this.clone().asInstanceOf

// Demo

class Person(private var _name: String, private var _age: Int) extends Rebuildable:
  def name = _name
  def name_=(n: String)(using BuildPhase) = _name = n

  def age = _age
  def age_=(a: Int)(using BuildPhase) = _age = a

object User:
  val guest = User(
    "Guest",
    "guest@app.com"
  )

class User(name: String, private var _email: String) extends Person(name, 18):
  def email = _email
  def email_=(e: String)(using BuildPhase) = _email = e

@main def demoRebuildable() =
  val first  = Person("The First", 1)
  val second = first.having(_.name = "The Second")
  val third  = second.having(_.age = 50)
  val error  = second.having(_ => first.name = "FirstMutated")
  //  ^^^^^ Object (first : sgl.util.Person) is past it's rebuilding phase.

  val fresh  = Person("Fresh", 18).having { _.name = "Anon" }

  println(first) // Person("The First", 1)
  println(second) // Person("The Second", 1)
  println(third) // Person("The Second", 36)
  println(fresh) // Person("Anon", 18)

  val user = User("Joe", "joe@gmail.com")
  val john = user.having { _.name += " Mama" }

  val newUser = User.guest.having { self =>
    self.name  = "Thomas Thomasson"
    self.email = "thomas.thomasson@pawnhub.com"
    self.age   = 25
  }

  val newUserAlt = User.guest.having(
    _.name  = "Thomas Thomasson",
    _.email = "thomas.thomasson.pawnhub.com",
    _.age   = 25
  )

  println(User.guest) // User("Guest", "guest@app.com", 18)
  println(user) // User("Joe", "joe@gmail.com")
  println(john) // User("Joe Mama", "joe@gmail.com")
  println(newUser) // User("Thomas Thomasson", "thomas.thomasson.pawnhub.com", 25)
  println(newUserAlt) // User("Thomas Thomasson", "thomas.thomasson.pawnhub.com", 25)

