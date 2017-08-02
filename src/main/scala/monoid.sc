import scalaz._, Scalaz._

trait Atom
case object Oxygen extends Atom
case object Hydrogen extends Atom
case object Barium extends Atom

implicit object MoleculeIsMonoid extends Monoid[Molecule] {
  def zero: Molecule = Molecule(Map(), 1)
  def append(f1: Molecule, f2: => Molecule): Molecule = f1.fusion(f2)
}

case class Molecule(atoms: Map[Atom, Int], stability: Double) {
  def fusion(other: Molecule): Molecule =
    Molecule(
      atoms = atoms |+| other.atoms,
      stability = stability * other.stability
    )
}

val m1 = Molecule(Map(Oxygen -> 1, Hydrogen -> 2), 0.2)
val m2 = Molecule(Map(Oxygen -> 1), 0.3)
val m3 = Molecule(Map(Oxygen -> 2, Hydrogen -> 1), 0.2)
val m4 = Molecule(Map(Oxygen -> 2, Barium -> 3), 0.5)

m1 |+| m2
//Molecule(Map(Oxygen -> 2, Hydrogen -> 2), 0.06)

m3 |+| m4 |+| m2
//Molecule = Molecule(Map(Oxygen -> 5, Barium -> 3, Hydrogen -> 1),0.03)

List(m1, m2, m2, m1).foldLeft(Molecule(Map(), 1))(_ |+| _)
//Molecule(Map(Oxygen -> 4, Hydrogen -> 4), 0.0036)

//Some(m1) |+| Some(m2)
//Some(Molecule(Map(Oxygen -> 2, Hydrogen -> 2), 0.06))